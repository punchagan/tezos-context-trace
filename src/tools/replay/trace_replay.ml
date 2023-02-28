(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2022 Tarides <contact@tarides.com>                     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

include Trace_replay_intf
open Tezos_context_trace_stats_summary
open Lwt.Syntax
module TzPervasives = Tezos_base.TzPervasives
module Def = Tezos_context_trace.Replay_actions

module List = Stdlib.List
(** Use List from Stdlib instead of the Tezos one. *)

module Option = Stdlib.Option
(** Use Option from Stdlib instead of the Tezos one. *)

(** Use failwith from Stdlib instead of the Tezos one. *)
let failwith = Stdlib.failwith

(** Prepare the directory where the stats will be exported. *)
let prepare_artefacts_dir path =
  let rec mkdir_p path =
    if Sys.file_exists path then ()
    else
      let path' = Filename.dirname path in
      if path' = path then failwith "Failed to prepare result dir";
      mkdir_p path';
      Unix.mkdir path 0o755
  in
  mkdir_p path

(** [with_progress_bar ~message ~n ~unit] will create a progress bar with
     a [message] displayed, using [unit] for values and, with [n] max
     elements. *)
let with_progress_bar ~message ~n ~unit =
  let open Progress in
  let config =
    Config.v ~max_width:(Some 79) ~min_interval:(Some Duration.(of_sec 0.5)) ()
  in
  let bar =
    Line.(
      list
        [
          const message;
          count_to n;
          const unit;
          elapsed ();
          parens (const "ETA: " ++ eta n);
          bar n;
          percentage_of n;
        ])
  in
  with_reporter ~config bar

let ( // ) = Filename.concat

let rec recursively_iter_files_in_directory f directory =
  Sys.readdir directory |> Array.to_list
  |> List.map (fun fname -> directory // fname)
  |> List.iter (fun p ->
         if Sys.is_directory p then recursively_iter_files_in_directory f p
         else f p)

let chmod_ro p = Unix.chmod p 0o444
let chmod_rw p = Unix.chmod p 0o644

let check_summary_mode config =
  let dir_path = config.artefacts_dir in
  let path = Filename.concat dir_path "stats_summary.json" in
  if Sys.file_exists path then
    match Unix.access path Unix.[ R_OK; W_OK ] with
    | () -> ()
    | exception Unix.Unix_error (e, _, _) -> failwith (Unix.error_message e)
  else
    match Unix.access dir_path [ R_OK; W_OK ] with
    | () -> ()
    | exception Unix.Unix_error (e, _, _) -> failwith (Unix.error_message e)

let exec_cmd cmd args =
  let cmd = Filename.quote_command cmd args in
  Logs.info (fun l -> l "Executing %s" cmd);
  let err = Sys.command cmd in
  if err <> 0 then Fmt.failwith "Got error code %d for %s" err cmd

let should_check_hashes config = config.empty_blobs = false

let open_reader max_block_count path =
  let version, header, ops_seq = Def.open_reader path in
  let block_count =
    match max_block_count with
    | None ->
        (* User didn't set a [block_count], read it from file. *)
        header.block_count
    | Some max_block_count ->
        (* User asked for a specific [block_count] let's clip it. *)
        if max_block_count > header.block_count then
          Logs.info (fun l ->
              l "Will only replay %d blocks instead of %d" header.block_count
                max_block_count);
        min max_block_count header.block_count
  in
  let aux (ops_seq, block_sent_count) =
    if block_sent_count >= block_count then None
    else
      match ops_seq () with
      | Seq.Nil ->
          Fmt.failwith
            "Reached the end of replayable trace while loading blocks idx %d. \
             The file was expected to contain %d blocks."
            block_sent_count header.block_count
      | Cons (row, ops_sec) -> Some (row, (ops_sec, block_sent_count + 1))
  in
  (version, block_count, header, Seq.unfold aux (ops_seq, 0))

module Make
    (Context : Tezos_context_disk.TEZOS_CONTEXT_UNIX)
    (Raw_config : Config) =
struct
  module type RECORDER = Tezos_context_trace.Recorder.S

  module Stat_recorder = Tezos_context_trace.Stats_recorder.Make (struct
    let prefix = Raw_config.v.artefacts_dir
    let message = Raw_config.v.stats_trace_message
  end)

  module Context = Tezos_context_disk_recorder.Make (struct
    module type RECORDER = RECORDER

    let l = [ (module Stat_recorder : RECORDER) ]
  end)

  type ('a, 'b) assoc = ('a * 'b) list

  type warm_replay_state = {
    config : config;
    index : Context.index;
    mutable contexts : (Optint.Int63.t, Context.t) assoc;
    mutable trees : (Optint.Int63.t, Context.tree) assoc;
    mutable hash_corresps : (Def.hash, Context_hash.t) assoc;
    check_hashes : bool;
    block_count : int;
    mutable current_block_idx : int;
    mutable current_row : Def.row;
    mutable current_event_idx : int;
    mutable recursion_depth : int;
    mutable latest_gc : Irmin_pack_unix.Stats.Latest_gc.stats option;
    mutable early_stop : bool;
    hash_per_level : (int, Context_hash.t) Stdlib.Hashtbl.t;
  }

  type cold_replay_state = { config : config; block_count : int }
  type cold = [ `Cold of cold_replay_state ]
  type warm = [ `Warm of warm_replay_state ]

  type t = [ cold | warm ]
  (** [t] is the type of the replay state.

      Before the very first operation is replayed (i.e. [init]) it is of type
      [cold]. After that operation, and until the end of the replay, it is of
      type [warm].

      The reason for this separation is that the [index] field of
      [warm_replay_state] is only available after the [init] operation.

      [warm_replay_state] is implemented using mutability, it could not be
      implemented with a fully functional scheme because we could not return an
      updated version when replaying [patch_context].

      The 3 dictionaries in [warm_replay_state] are implemented using [assoc]
      instead of [hashtbl] or [map] for performance reason -- these dictionaries
      rarely contain more that 1 element. *)

  let check_hash_trace hash_trace hash_replayed =
    let hash_replayed = Context_hash.to_string hash_replayed in
    if hash_trace <> hash_replayed then
      Fmt.failwith "hash replay %s, hash trace %s" hash_replayed hash_trace

  let get_ok = function
    | Error e -> Fmt.(str "%a" (list TzPervasives.pp) e) |> failwith
    | Ok h -> h

  let bad_result rs res_t expected result =
    let pp_res = Repr.pp res_t in
    let ev = rs.current_row.ops.(rs.current_event_idx) in
    Fmt.failwith
      "Cannot reproduce event idx %#d of block idx %#d (%a) expected %a for %a"
      rs.current_block_idx rs.current_block_idx (Repr.pp Def.event_t) ev pp_res
      expected pp_res result

  (** To be called each time lib_context procudes a tree *)
  let on_rhs_tree rs (scope_end, tracker) tree =
    match scope_end with
    | Def.Last_occurence -> ()
    | Will_reoccur -> rs.trees <- (tracker, tree) :: rs.trees

  (** To be called each time lib_context procudes a context *)
  let on_rhs_context rs (scope_end, tracker) context =
    match scope_end with
    | Def.Last_occurence -> ()
    | Will_reoccur -> rs.contexts <- (tracker, context) :: rs.contexts

  (** To be called each time lib_context procudes a commit hash *)
  let on_rhs_hash rs (scope_start, scope_end, hash_trace) hash_replayed =
    if rs.check_hashes then check_hash_trace hash_trace hash_replayed;
    match (scope_start, scope_end) with
    | Def.First_instanciation, Def.Last_occurence -> ()
    | First_instanciation, Will_reoccur ->
        rs.hash_corresps <- (hash_trace, hash_replayed) :: rs.hash_corresps
    | Reinstanciation, Last_occurence ->
        rs.hash_corresps <- List.remove_assoc hash_trace rs.hash_corresps
    | Reinstanciation, Will_reoccur ->
        (* This may occur if 2 commits of the replay have the same hash *)
        ()

  (** To be called each time a tree is passed to lib_context *)
  let on_lhs_tree rs (scope_end, tracker) =
    let v =
      List.assoc tracker rs.trees
      (* Shoudn't fail because it should follow a [save_context]. *)
    in
    if scope_end = Def.Last_occurence then
      rs.trees <- List.remove_assoc tracker rs.trees;
    v

  (** To be called each time a context is passed to lib_context *)
  let on_lhs_context rs (scope_end, tracker) =
    let v =
      List.assoc tracker rs.contexts
      (* Shoudn't fail because it should follow a [save_context]. *)
    in
    if scope_end = Def.Last_occurence then
      rs.contexts <- List.remove_assoc tracker rs.contexts;
    v

  (** To be called each time a commit hash is passed to lib_context *)
  let on_lhs_hash rs (scope_start, scope_end, hash_trace) =
    match (scope_start, scope_end) with
    | Def.Instanciated, Def.Last_occurence ->
        let v = List.assoc hash_trace rs.hash_corresps in
        rs.hash_corresps <- List.remove_assoc hash_trace rs.hash_corresps;
        v
    | Instanciated, Def.Will_reoccur -> List.assoc hash_trace rs.hash_corresps
    | Not_instanciated, (Def.Last_occurence | Def.Will_reoccur) ->
        (* This hash has not been seen yet out of a [commit] or [commit_genesis],
           this implies that [hash_trace] exist in the store prior to replay.

           The typical occurence of that situation is the first checkout of a
           replay starting from an existing store. *)
        Context_hash.of_string_exn hash_trace

  module Tree = struct
    let exec_empty rs (c0, tr) =
      let c0' = on_lhs_context rs c0 in
      let tr' = Context.Tree.empty c0' in
      on_rhs_tree rs tr tr';
      Lwt.return_unit

    let exec_of_value rs ((c0, v), tr) =
      let c0' = on_lhs_context rs c0 in
      let* tr' = Context.Tree.of_value c0' v in
      on_rhs_tree rs tr tr';
      Lwt.return_unit

    let exec_of_raw rs (raw, tr) =
      let rec conv = function
        | `Value _ as v -> v
        | `Tree bindings ->
            `Tree
              (bindings |> List.to_seq
              |> Seq.map (fun (k, v) -> (k, conv v))
              |> String.Map.of_seq)
      in
      let raw = conv raw in
      let tr' = Context.Tree.of_raw raw in
      on_rhs_tree rs tr tr';
      Lwt.return_unit

    let exec_mem rs ((tr, k), res) =
      let tr' = on_lhs_tree rs tr in
      let* res' = Context.Tree.mem tr' k in
      if res <> res' then bad_result rs Repr.bool res res';
      Lwt.return_unit

    let exec_mem_tree rs ((tr, k), res) =
      let tr' = on_lhs_tree rs tr in
      let* res' = Context.Tree.mem_tree tr' k in
      if res <> res' then bad_result rs Repr.bool res res';
      Lwt.return_unit

    let exec_find rs ((tr, k), res) =
      let tr' = on_lhs_tree rs tr in
      let* res' = Context.Tree.find tr' k in
      let res' = Option.is_some res' in
      if res <> res' then bad_result rs Repr.bool res res';
      Lwt.return_unit

    let exec_is_empty rs (tr, res) =
      let tr' = on_lhs_tree rs tr in
      let res' = Context.Tree.is_empty tr' in
      if res <> res' then bad_result rs Repr.bool res res';
      Lwt.return_unit

    let exec_kind rs (tr, res) =
      let tr' = on_lhs_tree rs tr in
      let res' = Context.Tree.kind tr' in
      if res <> res' then bad_result rs [%typ: [ `Tree | `Value ]] res res';
      Lwt.return_unit

    let exec_hash rs (tr, ()) =
      let tr' = on_lhs_tree rs tr in
      let (_ : Context_hash.t) = Context.Tree.hash tr' in
      Lwt.return_unit

    let exec_equal rs ((tr0, tr1), res) =
      let tr0' = on_lhs_tree rs tr0 in
      let tr1' = on_lhs_tree rs tr1 in
      let res' = Context.Tree.equal tr0' tr1' in
      if res <> res' then bad_result rs Repr.bool res res';
      Lwt.return_unit

    let exec_to_value rs (tr, res) =
      let tr' = on_lhs_tree rs tr in
      let* res' = Context.Tree.to_value tr' in
      let res' = Option.is_some res' in
      if res <> res' then bad_result rs Repr.bool res res';
      Lwt.return_unit

    let exec_clear rs ((depth, tr), ()) =
      let tr' = on_lhs_tree rs tr in
      Context.Tree.clear ?depth tr';
      Lwt.return_unit

    let exec_find_tree rs ((tr0, k), tr1_opt) =
      let tr0' = on_lhs_tree rs tr0 in
      let* tr1'_opt = Context.Tree.find_tree tr0' k in
      match (tr1_opt, tr1'_opt) with
      | Some tr1, Some tr1' ->
          on_rhs_tree rs tr1 tr1';
          Lwt.return_unit
      | None, None -> Lwt.return_unit
      | _ ->
          bad_result rs Repr.bool (Option.is_some tr1_opt)
            (Option.is_some tr1'_opt)

    let exec_add rs ((tr0, k, v), tr1) =
      let tr0' = on_lhs_tree rs tr0 in
      let* tr1' = Context.Tree.add tr0' k v in
      on_rhs_tree rs tr1 tr1';
      Lwt.return_unit

    let exec_add_tree rs ((tr0, k, tr1), tr2) =
      let tr0' = on_lhs_tree rs tr0 in
      let tr1' = on_lhs_tree rs tr1 in
      let* tr2' = Context.Tree.add_tree tr0' k tr1' in
      on_rhs_tree rs tr2 tr2';
      Lwt.return_unit

    let exec_remove rs ((tr0, k), tr1) =
      let tr0' = on_lhs_tree rs tr0 in
      let* tr1' = Context.Tree.remove tr0' k in
      on_rhs_tree rs tr1 tr1';
      Lwt.return_unit
  end

  let exec_find_tree rs ((c, k), tr_opt) =
    let c' = on_lhs_context rs c in
    let* tr'_opt = Context.find_tree c' k in
    match (tr_opt, tr'_opt) with
    | Some tr, Some tr' ->
        on_rhs_tree rs tr tr';
        Lwt.return_unit
    | None, None -> Lwt.return_unit
    | _ ->
        bad_result rs Repr.bool (Option.is_some tr_opt) (Option.is_some tr'_opt)

  let exec_add_tree rs ((c0, k, tr), c1) =
    let c0' = on_lhs_context rs c0 in
    let tr' = on_lhs_tree rs tr in
    let* c1' = Context.add_tree c0' k tr' in
    on_rhs_context rs c1 c1';
    Lwt.return_unit

  let exec_mem rs ((c, k), res) =
    let c' = on_lhs_context rs c in
    let* res' = Context.mem c' k in
    if res <> res' then bad_result rs Repr.bool res res';
    Lwt.return_unit

  let exec_mem_tree rs ((c, k), res) =
    let c' = on_lhs_context rs c in
    let* res' = Context.mem_tree c' k in
    if res <> res' then bad_result rs Repr.bool res res';
    Lwt.return_unit

  let exec_find rs ((c, k), res) =
    let c' = on_lhs_context rs c in
    let* res' = Context.find c' k in
    let res' = Option.is_some res' in
    if res <> res' then bad_result rs Repr.bool res res';
    Lwt.return_unit

  let exec_get_protocol rs (c, ()) =
    let c = on_lhs_context rs c in
    let* (_ : Protocol_hash.t) = Context.get_protocol c in
    Lwt.return_unit

  let exec_hash rs ((time, message, c), ()) =
    let time = Time.Protocol.of_seconds time in
    let c = on_lhs_context rs c in
    let (_ : Context_hash.t) = Context.hash ~time ?message c in
    Lwt.return_unit

  let exec_find_predecessor_block_metadata_hash rs (c, ()) =
    let c = on_lhs_context rs c in
    let* (_ : Block_metadata_hash.t option) =
      Context.find_predecessor_block_metadata_hash c
    in
    Lwt.return_unit

  let exec_find_predecessor_ops_metadata_hash rs (c, ()) =
    let c = on_lhs_context rs c in
    let* (_ : Operation_metadata_list_list_hash.t option) =
      Context.find_predecessor_ops_metadata_hash c
    in
    Lwt.return_unit

  let exec_get_test_chain rs (c, ()) =
    let c = on_lhs_context rs c in
    let* (_ : Test_chain_status.t) = Context.get_test_chain c in
    Lwt.return_unit

  let exec_exists rs (hash, res) =
    let hash = on_lhs_hash rs hash in
    let* res' = Context.exists rs.index hash in
    if res <> res' then bad_result rs Repr.bool res res';
    Lwt.return_unit

  let exec_add rs ((c0, k, v), c1) =
    let c0' = on_lhs_context rs c0 in
    let* c1' = Context.add c0' k v in
    on_rhs_context rs c1 c1';
    Lwt.return_unit

  let exec_remove rs ((c0, k), c1) =
    let c0' = on_lhs_context rs c0 in
    let* c1' = Context.remove c0' k in
    on_rhs_context rs c1 c1';
    Lwt.return_unit

  let exec_add_protocol rs ((c0, h), c1) =
    let h = Protocol_hash.of_string_exn h in
    let c0' = on_lhs_context rs c0 in
    let* c1' = Context.add_protocol c0' h in
    on_rhs_context rs c1 c1';
    Lwt.return_unit

  let exec_add_predecessor_block_metadata_hash rs ((c0, h), c1) =
    let h = Block_metadata_hash.of_string_exn h in
    let c0' = on_lhs_context rs c0 in
    let* c1' = Context.add_predecessor_block_metadata_hash c0' h in
    on_rhs_context rs c1 c1';
    Lwt.return_unit

  let exec_add_predecessor_ops_metadata_hash rs ((c0, h), c1) =
    let h = Operation_metadata_list_list_hash.of_string_exn h in
    let c0' = on_lhs_context rs c0 in
    let* c1' = Context.add_predecessor_ops_metadata_hash c0' h in
    on_rhs_context rs c1 c1';
    Lwt.return_unit

  let exec_add_test_chain rs ((c0, s), c1) =
    let c0' = on_lhs_context rs c0 in
    let* c1' = Context.add_test_chain c0' s in
    on_rhs_context rs c1 c1';
    Lwt.return_unit

  let exec_remove_test_chain rs (c0, c1) =
    let c0' = on_lhs_context rs c0 in
    let* c1' = Context.remove_test_chain c0' in
    on_rhs_context rs c1 c1';
    Lwt.return_unit

  let exec_fork_test_chain rs ((c0, protocol, expiration), c1) =
    let protocol = Protocol_hash.of_string_exn protocol in
    let expiration = Time.Protocol.of_seconds expiration in
    let c0' = on_lhs_context rs c0 in
    let* c1' = Context.fork_test_chain c0' ~protocol ~expiration in
    on_rhs_context rs c1 c1';
    Lwt.return_unit

  let exec_checkout rs (hash, c) =
    let hash = on_lhs_hash rs hash in
    let* c' = Context.checkout rs.index hash in
    let c' = match c' with None -> failwith "Checkout failed" | Some x -> x in
    on_rhs_context rs c c';
    Lwt.return_unit

  let exec_clear_test_chain rs (chain_id, ()) =
    let chain_id = Chain_id.of_string_exn chain_id in
    Context.clear_test_chain rs.index chain_id

  let exec_gc rs (hash, _res) =
    let hash = on_lhs_hash rs hash in
    Context.gc rs.index hash

  let exec_split rs = Context.split rs.index

  let exec_simple_event rs = function
    | Def.Tree ev -> (
        match ev with
        | Empty data -> Tree.exec_empty rs data
        | Of_raw data -> Tree.exec_of_raw rs data
        | Of_value data -> Tree.exec_of_value rs data
        | Mem data -> Tree.exec_mem rs data
        | Mem_tree data -> Tree.exec_mem_tree rs data
        | Find data -> Tree.exec_find rs data
        | Is_empty data -> Tree.exec_is_empty rs data
        | Kind data -> Tree.exec_kind rs data
        | Hash data -> Tree.exec_hash rs data
        | Equal data -> Tree.exec_equal rs data
        | To_value data -> Tree.exec_to_value rs data
        | Clear data -> Tree.exec_clear rs data
        | Find_tree data -> Tree.exec_find_tree rs data
        | Add data -> Tree.exec_add rs data
        | Add_tree data -> Tree.exec_add_tree rs data
        | Remove data -> Tree.exec_remove rs data)
    | Find_tree data -> exec_find_tree rs data
    | Add_tree data -> exec_add_tree rs data
    | Mem data -> exec_mem rs data
    | Mem_tree data -> exec_mem_tree rs data
    | Find data -> exec_find rs data
    | Get_protocol data -> exec_get_protocol rs data
    | Hash data -> exec_hash rs data
    | Find_predecessor_block_metadata_hash data ->
        exec_find_predecessor_block_metadata_hash rs data
    | Find_predecessor_ops_metadata_hash data ->
        exec_find_predecessor_ops_metadata_hash rs data
    | Get_test_chain data -> exec_get_test_chain rs data
    | Exists data -> exec_exists rs data
    | Add data -> exec_add rs data
    | Remove data -> exec_remove rs data
    | Add_protocol data -> exec_add_protocol rs data
    | Add_predecessor_block_metadata_hash data ->
        exec_add_predecessor_block_metadata_hash rs data
    | Add_predecessor_ops_metadata_hash data ->
        exec_add_predecessor_ops_metadata_hash rs data
    | Add_test_chain data -> exec_add_test_chain rs data
    | Remove_test_chain data -> exec_remove_test_chain rs data
    | Fork_test_chain data -> exec_fork_test_chain rs data
    | Checkout data -> exec_checkout rs data
    | Clear_test_chain data -> exec_clear_test_chain rs data
    | ( Fold_start _ | Fold_end | Fold_step_enter _ | Fold_step_exit _ | Init _
      | Commit_genesis_end _ | Commit_genesis_start _ | Commit _
      | Patch_context_exit _ | Patch_context_enter _ ) as ev ->
        Fmt.failwith "Got %a at %s" (Repr.pp Def.event_t) ev __LOC__
    | Gc data -> exec_gc rs data
    | Split -> exec_split rs

  let specs_of_row (row : Def.row) =
    Tezos_context_trace.Stats.Commit_op.
      {
        level = row.level;
        tzop_count = row.tzop_count;
        tzop_count_tx = row.tzop_count_tx;
        tzop_count_contract = row.tzop_count_contract;
        tz_gas_used = row.tz_gas_used;
        tz_storage_size = row.tz_storage_size;
        tz_cycle_snapshot = row.tz_cycle_snapshot;
        tz_time = row.tz_time;
        tz_solvetime = row.tz_solvetime;
        ev_count = Array.length row.ops;
        uses_patch_context = row.uses_patch_context;
      }

  let exec_commit_genesis rs ((chain_id, time, protocol), ()) =
    Stat_recorder.set_stat_specs (specs_of_row rs.current_row);
    let chain_id = Chain_id.of_string_exn chain_id in
    let time = Time.Protocol.of_seconds time in
    let protocol = Protocol_hash.of_string_exn protocol in
    (* Might execute [exec_init @ patch_context]. *)
    let* hash' = Context.commit_genesis ~time ~protocol ~chain_id rs.index in
    let hash' = get_ok hash' in

    let hash =
      match rs.current_row.ops.(rs.current_event_idx) with
      | Def.Commit_genesis_end ((), hash) -> hash
      | ev -> Fmt.failwith "Got %a at %s" (Repr.pp Def.event_t) ev __LOC__
    in

    on_rhs_hash rs hash hash';
    Lwt.return_unit

  module Event_sink_for_gc (X : sig
    val rs : warm_replay_state
  end) =
  struct
    type t = unit

    let uri_scheme = "context-replay"
    let configure _ = Lwt.return (Ok ())

    let should_handle ?section _sink _ =
      ignore section;
      false

    let handle (type a) () m ?section _v =
      ignore section;
      let module M = (val m : Internal_event.EVENT_DEFINITION with type t = a)
      in
      let () =
        match M.name with
        | "starting_gc" ->
            Fmt.epr "gc_started                    \n%!";
            Stat_recorder.report_gc_start ()
        | "ending_gc" ->
            Fmt.epr "gc_ended                     \n%!";
            let open Irmin_pack_unix.Stats in
            let latest_gc = (get ()).latest_gc |> Latest_gc.export in
            (match latest_gc with
            | None -> assert false
            | Some s -> Stat_recorder.report_gc s);
            X.rs.config.stop_after_nb_gc <- X.rs.config.stop_after_nb_gc - 1;
            if X.rs.config.stop_after_nb_gc <= 0 then X.rs.early_stop <- true
        | "gc_launch_failure" -> ()
        | "gc_failure" -> assert false
        | _ -> assert false
      in
      Lwt.return (Ok ())

    let close () = Lwt.return (Ok ())
  end

  module Commit_stats = struct
    let h = open_out "/tmp/tezos_replay_stats"

    type t = { mutable previous_timer : float; mutable times : float list }

    let t = { previous_timer = Unix.gettimeofday (); times = [] }
    let steps = 30.0
    let isteps = int_of_float steps
    let time0 = isteps * int_of_float (t.previous_timer /. steps)

    let print_stats () : unit =
      let arr = Array.of_list t.times in
      t.times <- [];
      Array.sort Float.compare arr;
      let len = Array.length arr in
      let q0 = arr.(0) in
      let q1 = arr.(len / 4) in
      let q2 = arr.(len / 2) in
      let q3 = arr.(len * 3 / 4) in
      let q4 = arr.(len - 1) in
      let time = isteps * int_of_float (t.previous_timer /. steps) in
      Printf.fprintf h "%i\t%i\t%f\t%f\t%f\t%f\t%f\n%!" (time - time0) len q0 q1
        q2 q3 q4

    let add () =
      let now = Unix.gettimeofday () in
      if int_of_float (t.previous_timer /. steps) <> int_of_float (now /. steps)
      then print_stats ();
      t.times <- (now -. t.previous_timer) :: t.times;
      t.previous_timer <- now
  end

  let exec_commit rs ((time, message, c), hash) =
    let level = rs.current_row.level in
    Stat_recorder.set_stat_specs (specs_of_row rs.current_row);
    let time = Time.Protocol.of_seconds time in
    let c = on_lhs_context rs c in
    let* hash' = Context.commit ~time ?message c in
    on_rhs_hash rs hash hash';
    Stdlib.Hashtbl.add rs.hash_per_level rs.current_block_idx hash';
    Commit_stats.add ();

    let gc_target_opt =
      match rs.config.gc_target with
      | `Distance i ->
          Stdlib.Hashtbl.find_opt rs.hash_per_level (rs.current_block_idx - i)
      | `Level i -> Stdlib.Hashtbl.find_opt rs.hash_per_level i
      | `Hash h -> Some h
    in
    match (rs.config.gc_when, gc_target_opt) with
    | `Never, _ -> Lwt.return_unit
    | `Every i, opt_target when rs.current_block_idx mod i = 0 -> (
        let idx = rs.current_block_idx in
        let* () = Context.split rs.index in
        rs.config.skip_gc <- rs.config.skip_gc - 1;
        match opt_target with
        | None ->
            Format.printf "@.Skip no target at %i@.@." idx;
            Lwt.return ()
        | Some target ->
            if rs.config.skip_gc > 0 then (
              Format.printf "@.Skip at %i@.@." idx;
              Lwt.return ())
            else (
              Format.printf "@.GC at %i@.@." idx;
              Context.gc rs.index target))
    | `Level lvl, Some target when lvl = level -> Context.gc rs.index target
    | `Level lvl, None when lvl = level ->
        Fmt.failwith "Should GC now but can't find target"
    | _ -> Lwt.return_unit

  let rec exec_init (rs : cold_replay_state) (row : Def.row) (readonly, ()) =
    let rsref = ref None in
    let patch_context c' =
      (* Will be called from [exec_commit_genesis] if
         [row.uses_patch_context = true]. *)
      let rs = Option.get !rsref in
      exec_patch_context rs c'
    in
    let patch_context =
      if row.uses_patch_context then Some patch_context else None
    in
    let store_dir = Filename.concat rs.config.artefacts_dir "store" in
    let* index =
      Context.init ~indexing_strategy:Raw_config.v.indexing_strategy ~readonly
        ?patch_context store_dir
    in
    let latest_gc =
      Irmin_pack_unix.Stats.((get ()).latest_gc |> Latest_gc.export)
    in
    let rs =
      {
        config = rs.config;
        index;
        contexts = [];
        trees = [];
        hash_corresps = [];
        check_hashes = should_check_hashes rs.config;
        block_count = rs.block_count;
        current_block_idx = 0;
        current_row = row;
        current_event_idx = 0;
        recursion_depth = 0;
        latest_gc;
        early_stop = false;
        hash_per_level = Stdlib.Hashtbl.create 0;
      }
    in
    let* () =
      let open Internal_event in
      All_sinks.register
        (module Event_sink_for_gc (struct
          let rs = rs
        end));
      let+ res = All_sinks.activate (Uri.of_string "context-replay://") in
      match res with Error _err -> assert false | Ok () -> ()
    in
    rsref := Some rs;
    Lwt.return rs

  and exec_patch_context rs c' =
    (match rs.current_row.ops.(rs.current_event_idx) with
    | Commit_genesis_end _ | Commit_genesis_start _ -> ()
    | ev -> Fmt.failwith "Got %a at %s" (Repr.pp Def.event_t) ev __LOC__);

    assert (rs.recursion_depth = 0);
    rs.recursion_depth <- 1;

    let* () =
      rs.current_event_idx <- rs.current_event_idx + 1;
      match rs.current_row.ops.(rs.current_event_idx) with
      | Def.Patch_context_enter c ->
          on_rhs_context rs c c';
          exec_next_events rs
      | ev -> Fmt.failwith "Got %a at %s" (Repr.pp Def.event_t) ev __LOC__
    in

    assert (rs.recursion_depth = 1);
    rs.recursion_depth <- 0;

    match rs.current_row.ops.(rs.current_event_idx) with
    | Patch_context_exit (c, d) ->
        let _c' : Context.t = on_lhs_context rs c in
        let d' = on_lhs_context rs d in
        rs.current_event_idx <- rs.current_event_idx + 1;
        Lwt.return (Ok d')
    | ev -> Fmt.failwith "Got %a at %s" (Repr.pp Def.event_t) ev __LOC__

  and exec_fold rs depth order c k =
    let c = on_lhs_context rs c in
    let f _k tr' () = exec_fold_step rs tr' in
    let* () = Context.fold ?depth ~order c k ~init:() ~f in

    rs.current_event_idx <- rs.current_event_idx + 1;
    match rs.current_row.ops.(rs.current_event_idx) with
    | Fold_end -> Lwt.return_unit
    | ev -> Fmt.failwith "Got %a at %s" (Repr.pp Def.event_t) ev __LOC__

  and exec_fold_step rs tr' =
    let recursion_depth = rs.recursion_depth in
    rs.recursion_depth <- recursion_depth + 1;

    let* () =
      rs.current_event_idx <- rs.current_event_idx + 1;
      match rs.current_row.ops.(rs.current_event_idx) with
      | Def.Fold_step_enter tr ->
          on_rhs_tree rs tr tr';
          exec_next_events rs
      | ev -> Fmt.failwith "Got %a at %s" (Repr.pp Def.event_t) ev __LOC__
    in

    assert (rs.recursion_depth = recursion_depth + 1);
    rs.recursion_depth <- recursion_depth;

    match rs.current_row.ops.(rs.current_event_idx) with
    | Fold_step_exit tr ->
        let _tr' : Context.tree = on_lhs_tree rs tr in
        Lwt.return_unit
    | ev -> Fmt.failwith "Got %a at %s" (Repr.pp Def.event_t) ev __LOC__

  and exec_next_events rs =
    rs.current_event_idx <- rs.current_event_idx + 1;
    let events = rs.current_row.Def.ops in
    let commit_idx = Array.length events - 1 in
    let i = rs.current_event_idx in
    let ev = events.(i) in
    match ev with
    | Def.Commit data ->
        assert (rs.recursion_depth = 0);
        assert (i = commit_idx);
        exec_commit rs data
    | Commit_genesis_start data ->
        assert (rs.recursion_depth = 0);
        exec_commit_genesis rs data
    | Fold_start ((x, x'), y, z) ->
        let* () = exec_fold rs x x' y z in
        (exec_next_events [@tailcall]) rs
    | Patch_context_exit (_, _) ->
        (* Will destack to [exec_patch_context] *)
        Lwt.return_unit
    | Fold_step_exit _ ->
        (* Will destack to [exec_fold_step] *)
        Lwt.return_unit
    | _ ->
        let* () = exec_simple_event rs ev in
        (exec_next_events [@tailcall]) rs

  let exec_block : [< t ] -> _ -> _ -> warm Lwt.t =
   fun t row block_idx ->
    let exec_very_first_event rs =
      assert (block_idx = 0);
      let events = row.Def.ops in
      let ev = events.(0) in
      match ev with
      | Def.Init data -> exec_init rs row data
      | ev -> Fmt.failwith "Got %a at %s" (Repr.pp Def.event_t) ev __LOC__
    in
    match t with
    | `Cold rs ->
        Logs.info (fun l ->
            l "exec block idx:%#6d, level:%#d, events:%#7d" block_idx
              row.Def.level (Array.length row.Def.ops));
        let* t = exec_very_first_event rs in
        let* () = exec_next_events t in
        Lwt.return (`Warm t)
    | `Warm (rs : warm_replay_state) ->
        let unusual_transition =
          List.length rs.trees > 0
          || List.length rs.contexts > 0
          || List.length rs.hash_corresps <> 1
        in
        if
          block_idx mod 250 = 0
          || block_idx + 1 = rs.block_count
          (* || Array.length row.Def.ops > 35_000 *)
          || unusual_transition
        then
          Logs.info (fun l ->
              let s =
                if unusual_transition then
                  Printf.sprintf "tree/context/hash caches:%d/%d/%d"
                    (List.length rs.trees) (List.length rs.contexts)
                    (List.length rs.hash_corresps)
                else ""
              in
              l "exec block idx:%#6d, level:%#d, events:%#7d %s" block_idx
                row.Def.level (Array.length row.Def.ops) s);
        rs.current_block_idx <- block_idx;
        rs.current_row <- row;
        rs.current_event_idx <- -1;
        let* () = exec_next_events rs in
        Lwt.return (`Warm rs)

  let exec_blocks (rs : cold_replay_state) row_seq : warm Lwt.t =
    with_progress_bar ~message:"Replaying trace" ~n:rs.block_count
      ~unit:"commits"
    @@ fun prog ->
    let rec aux t commit_idx row_seq =
      match row_seq () with
      | Seq.Nil -> (
          match t with `Cold _ -> assert false | `Warm _ as t -> Lwt.return t)
      | Cons (row, row_seq) ->
          let* (`Warm rs as t) = exec_block t row commit_idx in
          prog 1;
          if rs.early_stop then Lwt.return t else aux t (commit_idx + 1) row_seq
    in
    aux (`Cold rs) 0 row_seq

  let run () =
    let check_hashes = should_check_hashes Raw_config.v in
    let store_dir = Filename.concat Raw_config.v.artefacts_dir "store" in
    Logs.info (fun l ->
        l "Will %scheck commit hashes against reference."
          (if check_hashes then "" else "NOT "));
    Logs.info (fun l ->
        l "Will %skeep irmin store at the end."
          (if Raw_config.v.keep_store then "" else "NOT "));
    Logs.info (fun l ->
        l "Will %skeep stat trace at the end."
          (if Raw_config.v.keep_stats_trace then "" else "NOT "));
    Logs.info (fun l ->
        l "Will %ssave a custom message in stats trace."
          (if Raw_config.v.stats_trace_message <> None then "" else "NOT "));
    Logs.info (fun l ->
        l "Will %screate a summary file.\nWill %sprint a summary result."
          (if Raw_config.v.no_summary then "NOT " else " ")
          (if Raw_config.v.no_pp_summary then "NOT " else ""));
    prepare_artefacts_dir Raw_config.v.artefacts_dir;
    if Sys.file_exists store_dir then
      invalid_arg "Can't open irmin-pack store. Destination already exists";

    if not Raw_config.v.no_summary then check_summary_mode Raw_config.v;

    Logs.info (fun l -> l "Will use the Tezos GC to replay the trace.");
    let default_allocation_policy = 2 in
    let current = Gc.get () in
    Gc.set { current with allocation_policy = default_allocation_policy };

    (* 1. First open the replayable trace, *)
    let _, block_count, _, row_seq =
      open_reader Raw_config.v.block_count Raw_config.v.replayable_trace_path
    in
    let config = { Raw_config.v with block_count = Some block_count } in

    (match config.startup_store_type with
    | `Fresh -> ()
    | `Copy_from origin ->
        (* 2 - then make a copy of the reference RO store, *)
        exec_cmd "cp"
          [
            (* use -L to dereference symbolic links *)
            "-L";
            "-r";
            origin;
            store_dir;
          ];
        recursively_iter_files_in_directory chmod_rw store_dir);
    (* 4 - now launch the full replay, *)
    let* (`Warm replay_state) = exec_blocks { config; block_count } row_seq in

    (* 5 - and close the various things open, *)
    Logs.info (fun l -> l "Closing repo...");
    let+ () = Context.close replay_state.index in

    let res =
      if not config.no_summary then (
        Logs.info (fun l -> l "Computing summary...");
        (* 6 - compute the summary, *)
        let stats_path = Stat_recorder.get_stat_path () in
        Some
          (Trace_stats_summary.summarise ~info:(false, block_count, true)
             stats_path))
      else None
    in

    (* 7 - remove or preserve the various temporary files, *)
    let stats_path = Stat_recorder.get_stat_path () in
    if config.keep_stats_trace then (
      Logs.info (fun l -> l "Stats trace kept at %s" stats_path);
      chmod_ro stats_path)
    else Sys.remove stats_path;
    if config.keep_store then (
      Logs.info (fun l ->
          l "Store kept at %s" (Filename.concat config.artefacts_dir "store"));
      recursively_iter_files_in_directory chmod_ro store_dir)
    else exec_cmd "rm" [ "-rf"; store_dir ];

    match res with
    | Some summary ->
        (* 8 - and finally save and print the summary. *)
        let p = Filename.concat config.artefacts_dir "stats_summary.json" in
        Trace_stats_summary.save_to_json summary p;
        if not config.no_pp_summary then
          Logs.info (fun l ->
              l "\n%a" (Trace_stats_summary_pp.pp 5) ([ "" ], [ summary ]))
        else
          Logs.info (fun l ->
              l "No summary print as --no-pp-summary flag is true.")
    | None -> Logs.info (fun l -> l "No summary to print.")
end
