(* Copyright (C) 2014, 2015 Anthony Fox, University of Cambridge
 * Copyright (C) 2014, 2015 Alexandre Joannou, University of Cambridge
 * Copyright (C) 2015, SRI International.
 *
 * This software was developed by SRI International and the University
 * of Cambridge Computer Laboratory under DARPA/AFRL contract
 * FA8750-11-C-0249 ("MRC2"), as part of the DARPA MRC research
 * programme.
 *
 * See the LICENSE file for details.
 *)
(* --------------------------------------------------------------------------
   RISCV emulator
   -------------------------------------------------------------------------- *)

(* --------------------------------------------------------------------------
   Default Configuration
   -------------------------------------------------------------------------- *)

val be = ref false (* little-endian *)
val trace_level = ref 0
val time_run = ref true
val current_core_id = ref 0

(* --------------------------------------------------------------------------
   Utilities
   -------------------------------------------------------------------------- *)

fun hex s = L3.lowercase (BitsN.toHexString s)
fun phex n = StringCvt.padLeft #"0" (n div 4) o hex
fun word i = fn s => Option.valOf (BitsN.fromHexString (s, i))
val w8 = word 8
val w16 = word 16
val w32 = word 32
val w64 = word 64
val hex32 = phex 32
val hex64 = phex 64

fun failExit s = ( print (s ^ "\n"); OS.Process.exit OS.Process.failure )
fun err e s = failExit ("Failed to " ^ e ^ " file \"" ^ s ^ "\"")

fun debug_print s = print("==DEBUG== "^s)
fun debug_println s = print("==DEBUG== "^s^"\n")

(* --------------------------------------------------------------------------
   Loading code into memory from raw file
   -------------------------------------------------------------------------- *)

fun word8ToBits8 word8 =
    BitsN.B (Word8.toInt word8, 8)

fun getByte v i =
    if   i < Word8Vector.length v
    then word8ToBits8 (Word8Vector.sub (v, i))
    else BitsN.zero 8

(* TODO: this might be broken for big-endian code, but RISCV is
   little-endian by default. *)
fun storeVecInMemHelper vec base i =
    let val j = 8*i;
        val bytes0  = List.tabulate (8, fn inner => getByte vec (j+inner));
        val bytes1  = if !be then bytes0 else rev bytes0
        val bits64  = BitsN.concat bytes1
    in  if   j < Word8Vector.length vec
        then ( riscv.writeMem (BitsN.fromInt ((base + j), 64), bits64)
             ; storeVecInMemHelper vec base (i+1)
             )
        else
            print (Int.toString (Word8Vector.length vec) ^ " words.\n")
    end

fun storeVecInMem (base, memsz, vec) =
    let val vlen   = Word8Vector.length vec
        val padded = if memsz <= vlen then vec
                     else (
                         let val pad = Word8Vector.tabulate
                                           (memsz - vlen,  (fn _ => Word8.fromInt 0))
                         in  Word8Vector.concat (vec :: pad :: [])
                         end
                     )
    in  storeVecInMemHelper padded base 0
    end

fun printLog (n) = List.app (fn e => print(e ^ "\n"))
                            (List.rev(riscv.Map.lookup(!riscv.log, n)))

local
    fun readReg i = hex64 (riscv.GPR (BitsN.fromNat (i, 5)))
in
fun dumpRegisters (core) =
    let val savedProcID = !riscv.procID
        val pc = riscv.Map.lookup(!riscv.c_PC, 0)
        val () = riscv.procID := BitsN.B(core, BitsN.size (!riscv.procID))
    in  print "======   Registers   ======\n"
      ; print ("Core = " ^ Int.toString(core) ^ "\n")
      ; print ("PC     " ^ hex64 pc ^ "\n")
      ; L3.for
            (0, 31,
             fn i =>
                print ("Reg " ^ (if i < 10 then " " else "") ^
                       Int.toString i ^ " " ^ readReg i ^ "\n"))
      ; riscv.procID := savedProcID
    end
end

fun disassemble pc range =
    if range <= 0 then ()
    else let val word = riscv.readInst (BitsN.fromInt (IntInf.toInt pc, 64))
             val inst = riscv.Decode word
         in print ("0x" ^ (IntInf.fmt StringCvt.HEX pc) ^ ": "
                   ^ "0x" ^ hex32 word ^ ": "
                   ^ riscv.instructionToString inst
                   ^ "\n"
                  )
          ; disassemble (pc + 4) (range - 4)
         end

(* ------------------------------------------------------------------------
   Run code
   ------------------------------------------------------------------------ *)

fun log_loop mx i =
    let val () = riscv.procID := BitsN.B(!current_core_id,
                                         BitsN.size(!riscv.procID))
        val pc = riscv.Map.lookup(!riscv.c_PC, !current_core_id)
    in  riscv.instCnt := i
      ; riscv.Next ()
      ; printLog(0)
      ; if 1 <= !trace_level then printLog(1) else ()
      ; if 2 <= !trace_level then printLog(2) else ()
      ; if !riscv.done orelse i = mx
        then print ("Completed " ^ Int.toString (i + 1) ^ " instructions.\n")
        else log_loop mx (i + 1)
    end

fun decr i = if i <= 0 then i else i - 1

fun silent_loop mx =
    ( riscv.procID := BitsN.B(!current_core_id,
                              BitsN.size(!riscv.procID))
    ; riscv.Next ()
    ; if !riscv.done orelse (mx = 1) then (print "done\n")
      else silent_loop (decr mx)
    )

local
    fun t f x = if !time_run then Runtime.time f x else f x
in
fun run_mem mx =
    if   1 <= !trace_level
    then t (log_loop mx) 0
    else t silent_loop mx

fun run mx =
    run_mem mx
    handle riscv.UNPREDICTABLE s =>
           ( dumpRegisters
           ; failExit ("UNPREDICTABLE \"" ^ s ^ "\"\n")
           )
end

fun loadElf segs dis =
    List.app (fn s =>
                 if (#ptype s) = Elf.PT_LOAD
                 then ( print ( "Loading segment ...\n")
                      ; Elf.printPSeg s
                      ; storeVecInMem ((#vaddr s), (#memsz s), (#bytes s))
                      (* TODO: should check flags for executable segment *)
                      ; if dis then disassemble (#vaddr s) (#memsz s)
                        else ()
                      )
                 else ( print ("Skipping ")
                      ; Elf.printPSeg s
                      )
             ) segs

fun doElf cycles file dis =
    let val elf   = Elf.openElf file
        val hdr   = Elf.getElfHeader elf
        val psegs = Elf.getElfProgSegments elf hdr
    in
        riscv.procID    := BitsN.B(!current_core_id, BitsN.size(!riscv.procID))
      ; riscv.initRISCV (#entry hdr)
      ; riscv.totalCore := 1
      ; riscv.print     := debug_print
      ; riscv.println   := debug_println

      ; print "Loading elf file ...\n"
      ; Elf.printElfHeader hdr
      ; be := (if (#endian hdr = Elf.BIG) then true else false)
      ; loadElf psegs dis

      ; if dis
        then ( printLog (0)
             ; if 1 <= !trace_level then printLog(1) else ()
             ; if 2 <= !trace_level then printLog(2) else ()
             )
        else run cycles
    end

(* ------------------------------------------------------------------------
   Command line interface
   ------------------------------------------------------------------------ *)

local
    fun printUsage () =
        print
            ("\nRISCV emulator (based on an L3 specification).\n\
              \http://www.cl.cam.ac.uk/~acjf3/l3\n\n\
              \usage: " ^ OS.Path.file (CommandLine.name ()) ^ " [arguments] file\n\n\
              \Arguments:\n\
              \  --dis <bool>         only disassemble loaded code\n\
              \  --cycles <number>    upper bound on instruction cycles\n\
              \  --trace <level>      verbosity level (0 default, 2 maximum)\n\
              \  -h or --help         print this message\n\n")

    fun getNumber s =
        case IntExtra.fromString s of
            SOME n => n
          | NONE   => failExit ("Bad number: " ^ s)

    fun getBool s =
        case Bool.fromString s of
            SOME b => b
         |  NONE   => failExit ("Bad bool: " ^ s)

    fun getArguments () =
        List.map
            (fn "-c" => "--cycles"
            | "-t"   => "--trace"
            | "-d"   => "--dis"
            | "-h"   => "--help"
            | s      => s
            ) (CommandLine.arguments ())

    fun processOption (s: string) =
        let fun loop acc =
                fn a :: b :: r =>
                   if a = s
                   then (SOME b, List.rev acc @ r)
                   else loop (a :: acc) (b :: r)
              | r => (NONE, List.rev acc @ r)
        in loop []
        end
in
val () =
    case getArguments () of
        ["--help"] => printUsage ()
      | l =>
        let val (c, l) = processOption "--cycles" l
            val c = Option.getOpt (Option.map getNumber c, ~1)
            val (t, l) = processOption "--trace" l
            val t = Option.getOpt (Option.map getNumber t, !trace_level)
            val () = trace_level := Int.max (0, t)
            val (d, l) = processOption "--dis" l
            val d = Option.getOpt (Option.map getBool d, false)
        in
            if List.null l then printUsage ()
            else doElf c (List.hd l) d
        end
end
