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

val init : unit -> unit =
    buildCall0 (getSym "tv_init", (), cVoid)

val loadElf : string -> unit =
    buildCall1 (getSym "tv_load_elf", cString, cVoid)

val setVerbose : bool -> unit =
    buildCall1 (getSym "tv_set_verbose", cInt, cVoid) o fromBool

val reset : unit -> unit =
    buildCall0 (getSym "tv_reset", (), cVoid)

val isDone : unit -> bool =
    toBool o buildCall0 (getSym "tv_is_done", (), cInt)

val checkPC : IntInf.int -> bool =
    toBool o buildCall1 (getSym "tv_check_pc", cUint64, cInt)
    o IntInf.toInt

val checkPriv : riscv.Privilege -> bool =
    toBool o buildCall1 (getSym "tv_check_priv", cUint8, cInt)
    o (fn p => case p of riscv.User       => 0
                      |  riscv.Supervisor => 1
                      |  riscv.Machine    => 3)

val checkGPR : (int * IntInf.int) -> bool =
    toBool o buildCall2 (getSym "tv_check_gpr", (cUint64, cUint64), cInt)
    o (fn (rno, rval) => (rno, IntInf.toInt rval))

val checkCSR : (int * IntInf.int) -> bool =
    toBool o buildCall2 (getSym "tv_check_csr", (cUint64, cUint64), cInt)
    o (fn (rno, rval) => (rno, IntInf.toInt rval))

end
