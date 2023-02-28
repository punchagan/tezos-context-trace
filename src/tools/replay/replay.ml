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

open Cmdliner

let deprecated_info = (Term.info [@alert "-deprecated"])
let deprecated_eval = (Term.eval [@alert "-deprecated"])
let deprecated_exit = (Term.exit [@alert "-deprecated"])

type indexing_strategy = Always | Minimal | Contents

let main indexing_strategy block_count startup_store_type replayable_trace_path
    artefacts_dir keep_store keep_stats_trace no_summary empty_blobs
    stats_trace_message no_pp_summary gc_when gc_target stop_after_nb_gc skip_gc =
  let startup_store_type =
    match startup_store_type with None -> `Fresh | Some v -> `Copy_from v
  in
  let indexing_strategy =
    match indexing_strategy with
    | Always -> `Always
    | Minimal -> `Minimal
    | Contents -> `Minimal
  in
  let module Replay =
    Trace_replay.Make
      (Tezos_context.Context)
      (struct
        let v : Trace_replay.config =
          {
            block_count;
            startup_store_type;
            replayable_trace_path;
            artefacts_dir;
            keep_store;
            keep_stats_trace;
            no_summary;
            empty_blobs;
            stats_trace_message;
            no_pp_summary;
            indexing_strategy;
            gc_when;
            gc_target;
            stop_after_nb_gc;
            skip_gc;
          }
      end)
  in
  Lwt_main.run (Replay.run ())

let indexing_strategy =
  let doc = "Specify the indexing_strategy to run when doing the replay." in
  let strategy =
    Arg.enum
      [ ("always", Always); ("minimal", Minimal); ("contents", Contents) ]
  in
  Arg.(
    required
    & opt (some strategy) (Some Minimal)
    & info [ "s"; "indexing-strategy" ] ~doc)

let block_count =
  let doc =
    Arg.info ~doc:"Maximum number of blocks to read from trace."
      [ "block-count" ]
  in
  Arg.(value @@ opt (some int) None doc)

let startup_store_type =
  let doc =
    Arg.info ~docv:"PATH"
      ~doc:
        "Path to a directory that contains and irmin-pack store (i.e Tezos \
         data context directory) that shall serve as a basis for replay. The \
         provided path is not modified. If not provided, the replay starts \
         from a fresh store and the input actions trace should begin from the \
         genesis block."
      [ "startup-store-copy" ]
  in
  Arg.(value @@ opt (some string) None doc)

let replayable_trace_path =
  let doc =
    Arg.info ~docv:"TRACE_PATH"
      ~doc:"Trace of Tezos context operations to be replayed." []
  in
  Arg.(required @@ pos 0 (some string) None doc)

let artefacts_dir =
  let doc =
    Arg.info ~docv:"ARTEFACTS_PATH" ~doc:"Destination of the bench artefacts."
      []
  in
  Arg.(required @@ pos 1 (some string) None doc)

let keep_store =
  let doc =
    Arg.info
      ~doc:
        "Whether or not the irmin store should be discarded at the end. If \
         kept, the store will remain in the [ARTEFACTS_PATH] directory."
      [ "keep-store" ]
  in
  Arg.(value @@ flag doc)

let keep_stats_trace =
  let doc =
    Arg.info
      ~doc:
        "Whether or not the stats trace should be discarded are the end, after \
         the summary has been saved the disk. If kept, the stats trace will \
         remain in the [ARTEFACTS_PATH] directory"
      [ "keep-stats-trace" ]
  in
  Arg.(value @@ flag doc)

let no_summary =
  let doc =
    Arg.info
      ~doc:
        "Whether or not the stats trace should be converted to a summary at \
         the end of a replay."
      [ "no-summary" ]
  in
  Arg.(value @@ flag doc)

let empty_blobs =
  let doc =
    Arg.info
      ~doc:
        "Whether or not the blobs added to the store should be the empty \
         string, during trace replay. This greatly increases the replay speed."
      [ "empty-blobs" ]
  in
  Arg.(value @@ flag doc)

let stats_trace_message =
  let doc =
    Arg.info ~docv:"MESSAGE"
      ~doc:
        "Raw text to be stored in the header of the stats trace. Typically, a \
         JSON-formatted string that describes the setup of the benchmark(s)."
      [ "stats-trace-message" ]
  in
  Arg.(value @@ opt (some string) None doc)

let no_pp_summary =
  let doc =
    Arg.info
      ~doc:
        "Whether or not the summary should be displayed at the end of a replay."
      [ "no-pp-summary" ]
  in
  Arg.(value @@ flag doc)

let gc_when =
  let parser s =
    let fail () = Error (`Msg (Fmt.str "%S" s)) in
    match Stdlib.String.split_on_char '-' s with
    | [ "never" ] -> Ok `Never
    | [ "every"; d ] -> (
        match int_of_string_opt d with
        | None -> fail ()
        | Some d -> Ok (`Every d))
    | [ "after"; "level"; d ] -> (
        match int_of_string_opt d with
        | None -> fail ()
        | Some d -> Ok (`Level d))
    | _ -> fail ()
  in
  let printer ppf = function
    | `Never -> Fmt.pf ppf "never"
    | `Every d -> Fmt.pf ppf "every-%d" d
    | `Level d -> Fmt.pf ppf "level-%d" d
  in
  let doc = Arg.info ~doc:"When to start GCs." [ "gc-when" ] in
  Arg.(value @@ opt (conv (parser, printer)) `Never doc)

let gc_target =
  let parser s =
    let fail () = Error (`Msg (Fmt.str "%S" s)) in
    match Stdlib.String.split_on_char '-' s with
    | [ "distance"; d ] -> (
        match int_of_string_opt d with
        | None -> fail ()
        | Some d -> Ok (`Distance d))
    | [ "level"; d ] -> (
        match int_of_string_opt d with
        | None -> fail ()
        | Some d -> Ok (`Level d))
    | [ "hash"; s ] -> (
        match Context_hash.of_b58check_opt s with
        | None -> fail ()
        | Some h -> Ok (`Hash h))
    | _ -> fail ()
  in
  let printer ppf = function
    | `Distance d -> Fmt.pf ppf "distance-%d" d
    | `Level d -> Fmt.pf ppf "level-%d" d
    | `Hash h -> Fmt.pf ppf "hash-%s" (Context_hash.to_b58check h)
  in
  let doc = Arg.info ~doc:"Target of GCs." [ "gc-target" ] in
  Arg.(value @@ opt (conv (parser, printer)) (`Distance (8191 * 6)) doc)

let stop_after_nb_gc =
  let doc =
    Arg.info ~doc:"Whether or not the replay should stop after the first GC."
      [ "stop-after-nb-gc" ]
  in
  Arg.(value @@ opt int max_int @@ doc)

let skip_gc =
  let doc = Arg.info ~doc:"Skip the GC for nb rounds" ["skip-gc"] in
  Arg.(value @@ opt int 0 @@ doc)

let main_t =
  Term.(
    const main $ indexing_strategy $ block_count $ startup_store_type
    $ replayable_trace_path $ artefacts_dir $ keep_store $ keep_stats_trace
    $ no_summary $ empty_blobs $ stats_trace_message $ no_pp_summary $ gc_when
    $ gc_target $ stop_after_nb_gc $ skip_gc)

let () =
  let info =
    deprecated_info ~doc:"Replay operation from a raw actions trace." "replay"
  in
  let res = deprecated_eval (main_t, info) in
  deprecated_exit res
