(* Copyright (C) 2018 SRI International.
 *
 * This software was developed by SRI International and the University of
 * Cambridge Computer Laboratory under DARPA/AFRL contract FA8650-18-C-7809
 * ("CIFV").
 *
 * See the LICENSE file for details.
 *)

(*
 * This interface allows checking the model against an external oracle like the
 * Spike simulator.
 *)

signature Oracle =
sig
    type t

    (* initialization and lifecycle *)
    val init       : string     -> t
    val setVerbose : t * bool   -> unit
    val loadElf    : t * string -> unit (* TODO: cross-check the parsed entry point? *)
    val reset      : t          -> unit
    val step       : t          -> unit
    val isDone     : t          -> bool

    (* query the oracle's mmu configuration *)
    val isDirtyEnabled      : t -> bool
    val isMisalignedEnabled : t -> bool

    (* checks *)
    val checkPC    : t * IntInf.int       -> bool
    val checkPriv  : t * riscv.Privilege  -> bool
    val checkGPR   : t * int * IntInf.int -> bool
    val checkCSR   : t * int * IntInf.int -> bool
end
