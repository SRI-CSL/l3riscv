(* Copyright (C) 2014, 2015 Anthony Fox, University of Cambridge
 * Copyright (C) 2014, 2015 Alexandre Joannou, University of Cambridge
 * Copyright (C) 2015-2018  SRI International.
 *
 * This software was developed by SRI International and the University of
 * Cambridge Computer Laboratory under DARPA/AFRL contract FA8750-11-C-0249
 * ("MRC2"), as part of the DARPA MRC research programme, and under
 * DARPA/AFRL contract FA8750-10-C-0237 ("CTSRD"), as part of the DARPA
 * CRASH research programme, under DARPA/AFRL contract FA8650-18-C-7809
 * ("CIFV"), and under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of
 * the DARPA SSITH research programme.
 *
 * See the LICENSE file for details.
 *)

(* Outer shell for Poly/ML. *)

val L3_LIB_PATH =
  let
    val tmp = OS.FileSys.tmpName ()
  in
    if OS.Process.isSuccess (OS.Process.system ("l3 --lib-path > " ^ tmp))
       then let
              val strm = TextIO.openIn tmp
              val s = TextIO.inputAll strm
                      before (TextIO.closeIn strm; OS.FileSys.remove tmp)
            in
              OS.Path.fromString (String.substring (s, 0, String.size s - 1))
            end
    else raise Fail "Failed to get L3_LIB_PATH"
  end

local
   fun path {vol, isAbs, arcs} s e =
     let
       val f = OS.Path.joinBaseExt {base = s, ext = SOME e}
       val s = OS.Path.toString {vol = vol, isAbs = isAbs, arcs = arcs @ [f]}
     in
       if OS.FileSys.access (s, [OS.FileSys.A_READ]) then (
           (* print ("Using: " ^ s ^ "\n"); *)
           PolyML.use s
       ) else (
           (* print ("Not using: " ^ s ^ "\n"); *)
           ()
       )
     end
in
   fun useSigSml a s = List.app (path a s) ["sig", "sml"]
end

structure Real64           = LargeReal
structure PackReal64Little = PackRealLittle

val () =
  List.app (useSigSml L3_LIB_PATH)
    ["IntExtra", "Nat", "Set", "L3", "Bitstring", "BitsN", "Ptree", "Map",
     "MutableMapFunctor", "MutableMap16", "Runtime"];

val () = L3.setLibDir (OS.Path.toString L3_LIB_PATH);

val () =
  List.app (useSigSml L3_LIB_PATH)
     ["SSE", "PolySSE", "FP", "FP32", "FP64", "FPConvert"];

val () =
    let
        val cur_file = PolyML.getUseFileName ()
        val cur_dir  = (case cur_file of
                            NONE   => "."
                         |  SOME p => OS.Path.dir p)
        val cur_path = OS.Path.fromString cur_dir
        val lib_dir  = OS.Path.joinDirFile {dir = cur_dir, file = "lib"}
        val lib_path = OS.Path.fromString lib_dir
    in
        (* (case cur_file of
             NONE   => print ("No use-file found for main module\n")
          |  SOME p => print ("Cur file: '" ^ p ^ "'\n")); *)
        useSigSml lib_path "Elf";
        List.app (useSigSml cur_path) ["riscv", "oracle", "poly_spike", "model", "poly_model"]
    end
