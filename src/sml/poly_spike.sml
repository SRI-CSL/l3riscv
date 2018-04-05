(* Copyright (C) 2018 SRI International.
 *
 * This software was developed by SRI International and the University of
 * Cambridge Computer Laboratory under DARPA/AFRL contract FA8650-18-C-7809
 * ("CIFV").
 *
 * See the LICENSE file for details.
 *)

structure Oracle :> Oracle =
struct

open Foreign

type t = Memory.voidStar

(*
 * The code in this library is executed at compile-time, due to the
 * way Poly/ML compiler works.  So, loadLibrary of tv_spike.so is actually
 * performed during compilation.  In l3riscv/Makefile, we ensure that
 * tv_spike.so has already been built and placed in the proper location,
 * so that the loadLibrary succeeds when this file is compiled.
 * We are assuming that getDir() returns the location of the Makefile,
 * which works even when built using 'make -C ...'.
 *)
fun getLib () =
    loadLibrary (OS.Path.joinDirFile {dir  = OS.FileSys.getDir (),
                                      file = "tv_spike.so"})

val getSym = getSymbol (getLib ())

fun toBool i   = if i = 0 then false else true
fun fromBool b = if b     then 1     else 0

val init : string -> t =
    buildCall1 (getSym "tv_init", cString, cPointer)

val setVerbose : (t * bool) -> unit =
    buildCall2 (getSym "tv_set_verbose", (cPointer, cInt), cVoid)
    o (fn (t, b) => (t, fromBool b))

val isDirtyEnabled : t -> bool =
    toBool o buildCall1 (getSym "tv_is_dirty_enabled", cPointer, cInt)

val isMisalignedEnabled : t -> bool =
    toBool o buildCall1 (getSym "tv_is_misaligned_enabled", cPointer, cInt)

val isDone : t -> bool =
    toBool o buildCall1 (getSym "tv_is_done", cPointer, cInt)

val loadElf : (t * string) -> unit =
    buildCall2 (getSym "tv_load_elf", (cPointer, cString), cVoid)

val reset : t -> unit =
    buildCall1 (getSym "tv_reset", cPointer, cVoid)

val step : t -> unit =
    buildCall1 (getSym "tv_step", cPointer, cVoid)

val isDone : t -> bool =
    toBool o buildCall1 (getSym "tv_is_done", cPointer, cInt)

val checkPC : (t * IntInf.int) -> bool =
    toBool o buildCall2 (getSym "tv_check_pc", (cPointer, cUint64Large), cInt)
    o (fn (t, i) => (t, IntInf.toLarge i))

val checkPriv : (t * riscv.Privilege) -> bool =
    toBool o buildCall2 (getSym "tv_check_priv", (cPointer, cUint8), cInt)
    o (fn (t, p) => (t, case p of riscv.User       => 0
                               |  riscv.Supervisor => 1
                               |  riscv.Machine    => 3))

val checkGPR : (t * int * IntInf.int) -> bool =
    toBool o buildCall3 (getSym "tv_check_gpr", (cPointer, cUint64, cUint64Large), cInt)
    o (fn (t, rno, rval) => (t, rno, IntInf.toLarge rval))

val checkCSR : (t * int * IntInf.int) -> bool =
    toBool o buildCall3 (getSym "tv_check_csr", (cPointer, cUint64, cUint64Large), cInt)
    o (fn (t, rno, rval) => (t, rno, IntInf.toLarge rval))

end
