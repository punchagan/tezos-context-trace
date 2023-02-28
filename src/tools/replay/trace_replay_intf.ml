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

type config = {
  block_count : int option;
  startup_store_type : [ `Fresh | `Copy_from of string ];
  replayable_trace_path : string;
  artefacts_dir : string;
  keep_store : bool;
  keep_stats_trace : bool;
  no_summary : bool;
  empty_blobs : bool;
  stats_trace_message : string option;
  no_pp_summary : bool;
  indexing_strategy : [ `Always | `Minimal ];
  gc_when : [ `Never | `Every of int | `Level of int ];
  gc_target : [ `Distance of int | `Level of int | `Hash of Context_hash.t ];
  mutable stop_after_nb_gc : int;
  mutable skip_gc : int;
}

module type Config = sig
  val v : config
end

module type Sigs = sig
  type nonrec config = config

  module Make (Impl : Tezos_context_disk.TEZOS_CONTEXT_UNIX) (Conf : Config) : sig
    val run : unit -> unit Lwt.t
  end
end
