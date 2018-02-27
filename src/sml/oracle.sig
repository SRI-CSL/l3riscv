(* Copyright (C) 2018, SRI International.
 *
 * This software was developed by SRI International and the University of
 * Cambridge Computer Laboratory under DARPA/AFRL contract FA8750-11-C-0249
 * ("MRC2"), as part of the DARPA MRC research programme, and under
 * DARPA/AFRL contract FA8750-10-C-0237 ("CTSRD"), as part of the DARPA
 * CRASH research programme.
 *
 * See the LICENSE file for details.
 *)

(*
 * This interface allows checking the model against an external oracle like the
 * Spike simulator.
 *)

signature Oracle =
sig
    (* initialization and lifecycle *)
    val init       : unit -> unit
    val setVerbose : bool -> unit
    val loadElf    : string -> unit (* TODO: cross-check the parsed entry point? *)
    val isDone     : unit -> bool

    (* checks *)
    val checkPC   : IntInf.int       -> bool
    val checkPriv : riscv.Privilege  -> bool
    val checkGPR  : int * IntInf.int -> bool
    val checkCSR  : int * IntInf.int -> bool
end
