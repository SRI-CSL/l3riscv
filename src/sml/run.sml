(* Copyright (C) 2014, 2015 Anthony Fox, University of Cambridge
 * Copyright (C) 2014, 2015 Alexandre Joannou, University of Cambridge
 * Copyright (C) 2015, SRI International.
 *
 * This software was developed by SRI International and the University of
 * Cambridge Computer Laboratory under DARPA/AFRL contract FA8750-11-C-0249
 * ("MRC2"), as part of the DARPA MRC research programme, and under
 * DARPA/AFRL contract FA8750-10-C-0237 ("CTSRD"), as part of the DARPA
 * CRASH research programme.
 *
 * See the LICENSE file for details.
 *)

(* --------------------------------------------------------------------------
   RISCV emulator
   -------------------------------------------------------------------------- *)

(* Default Configuration *)

val mem_base_addr   = ref 0
val mem_size        = ref 4000000

(* Execution parameters *)

(* true  -> init starting PC to reset vector
   false -> use start offset from ELF *)
val boot        = ref true

val be          = ref false (* little-endian *)
val time_run    = ref true

val trace_level = ref 0
val trace_elf   = ref false

val verify          = ref false
val verify_exit_pc  = ref (Int64.fromInt (~1))

(* Utilities *)

fun hex s = L3.lowercase (BitsN.toHexString s)
fun phex n = StringCvt.padLeft #"0" (n div 4) o hex
val hex32 = phex 32
val hex64 = phex 64

fun failExit s = ( print (s ^ "\n"); OS.Process.exit OS.Process.failure )
fun err e s = failExit ("Failed to " ^ e ^ " file \"" ^ s ^ "\"")

fun debugPrint s = print("==DEBUG== "^s)
fun debugPrintln s = print("==DEBUG== "^s^"\n")

(* Bit vector utilities *)

fun word8ToBits8 word8 =
    BitsN.B (Word8.toInt word8, 8)

fun getByte v i =
    if   i < Word8Vector.length v
    then word8ToBits8 (Word8Vector.sub (v, i))
    else BitsN.zero 8

(* Memory utilities *)

(* TODO: this might be broken for big-endian code, but RISCV is
   little-endian by default. *)
fun storeVecInMemHelper vec base i =
    let val j = 8*i;
        val bytes0  = List.tabulate (8, fn inner => getByte vec (j+inner));
        val bytes1  = if !be then bytes0 else rev bytes0
        val bits64  = BitsN.concat bytes1
    in  if   j < Word8Vector.length vec
        then ( riscv.rawWriteMem (BitsN.fromInt ((base + j), 64), bits64)
             ; storeVecInMemHelper vec base (i+1)
             )
        else
            if !trace_elf
            then print (Int.toString (Word8Vector.length vec) ^ " words.\n")
            else ()
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

(* Printing utilities *)

fun printLog (n) = List.app (fn e => print(e ^ "\n"))
                            (List.rev(riscv.Map.lookup(!riscv.log, n)))

fun logLevels l =
    if l <= !trace_level
    then ( printLog(l) ; logLevels(l+1) )
    else ()

local
    fun readReg i = hex64 (riscv.GPR (BitsN.fromNat (i, 5)))
in
fun dumpRegisters core =
    let val savedProcID = !riscv.procID
        val pc = riscv.Map.lookup(!riscv.c_PC, 0)
        val () = riscv.procID := BitsN.B(core, BitsN.size (!riscv.procID))
    in  print "======   Registers   ======\n"
      ; print ("Core = " ^ Int.toString(core) ^ "\n")
      ; let val w = #pc_instr (riscv.Delta ())
            val i = riscv.Decode w
        in  print ("Faulting instruction: (0x" ^ hex32 w ^ ") "
                   ^ riscv.instructionToString i
                   ^ "\n\n")
        end
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
    else let val addr = BitsN.fromInt (IntInf.toInt pc, 64)
             val word = riscv.rawReadInst (addr)
             val inst = riscv.Decode word
         in print ("0x" ^ (L3.padLeftString(#"0", (10, BitsN.toHexString addr)))
                   ^ ": 0x" ^ hex32 word
                   ^ ": " ^ riscv.instructionToString inst
                   ^ "\n"
                  )
          ; disassemble (pc + 4) (range - 4)
         end

(* Tandem verification *)

val oracle_reset = _import "oracle_reset" : (Int64.int * Int64.int) -> unit;
fun initVerify () =
    oracle_reset (!mem_base_addr, !mem_size)

val oracle_load     = _import "oracle_load" : string -> unit;
val oracle_get_exit = _import "oracle_get_exit_pc" : unit -> Int64.int;
fun loadVerify filename =
    ( oracle_load filename
    ; verify_exit_pc := oracle_get_exit ()
    ; print ("Set exit pc to 0x" ^ Int64.fmt StringCvt.HEX (!verify_exit_pc) ^ "\n")
    )

val oracle_verify =
    _import "oracle_verify" : (bool
                               * Int64.int * Int64.int * Int64.int
                               * Int64.int * Int64.int * Int64.int
                               * Int32.int) -> bool;
fun doVerify () =
    let val delta       = riscv.Delta ()
        val exc_taken   = #exc_taken delta
        val pc          = Int64.fromInt (BitsN.toInt (#pc      delta))
        val addr        = Int64.fromInt (BitsN.toInt (#addr    delta))
        val data1       = Int64.fromInt (BitsN.toInt (#data1   delta))
        val data2       = Int64.fromInt (BitsN.toInt (#data2   delta))
        val data3       = Int64.fromInt (BitsN.toInt (#data3   delta))
        val fp_data     = Int64.fromInt (BitsN.toInt (#fp_data delta))
        val verbosity   = Int32.fromInt (!trace_level)
    in
        if oracle_verify (exc_taken, pc, addr, data1, data2, data3, fp_data, verbosity)
        then ()
        else ( print "Verification error:\n"
             ; dumpRegisters (BitsN.toInt (!riscv.procID))
             ; failExit "Verification FAILED!\n"
             )
    end

fun isVerifyDone () =
    if !verify then
        let val pc   = BitsN.toInt (riscv.Map.lookup(!riscv.c_PC, 0))
            val pc64 = Int64.fromInt pc
        in  Int64.compare (!verify_exit_pc, pc64) = EQUAL
        end
    else false

(* Code execution *)

fun currentCore () =
    BitsN.toInt (!riscv.procID)

fun nextCoreToSchedule () =
    (1 + currentCore ()) mod !riscv.totalCore

fun isLastCore () =
    BitsN.toInt (!riscv.procID) + 1 = !riscv.totalCore

fun logLoop mx i =
    ( riscv.scheduleCore (nextCoreToSchedule ())
    ; riscv.Next ()
    ; print ("\n")
    ; logLevels (0)
    ; if !verify then doVerify() else ()
    ; if !riscv.done orelse i = mx orelse isVerifyDone ()
      then ( print ("ExitCode: " ^ Nat.toString (riscv.exitCode ()) ^ "\n")
           ; print ("Completed " ^ Int.toString (i + 1) ^ " instructions.\n")
           )
      else logLoop mx (if isLastCore () then (i + 1) else i)
    )

fun decr i = if i <= 0 then i else i - 1

fun silentLoop mx =
    ( riscv.scheduleCore (nextCoreToSchedule ())
    ; riscv.Next ()
    ; if !verify then doVerify() else ()
    ; if !riscv.done orelse (mx = 1) orelse isVerifyDone ()
      then let val ec = riscv.exitCode ()
           in print ("done: exit code " ^ Nat.toString ec ^ "\n")
            ; OS.Process.exit (if ec = 0
                               then OS.Process.success
                               else OS.Process.failure)
           end
      else silentLoop (if isLastCore () then (decr mx) else mx)
    )

local
    fun t f x = if !time_run then Runtime.time f x else f x
in
fun run mx =
    if   1 <= !trace_level
    then t (logLoop mx) 0
    else t silentLoop mx

fun runWrapped mx =
    run mx
    handle riscv.UNDEFINED s =>
           ( dumpRegisters (currentCore ())
           ; failExit ("UNDEFINED \"" ^ s ^ "\"\n")
           )
         | riscv.INTERNAL_ERROR s =>
           ( dumpRegisters (currentCore ())
           ; failExit ("INTERNAL_ERROR \"" ^ s ^ "\"\n")
           )
end

fun loadElf segs dis =
    List.app (fn s =>
                 if (#ptype s) = Elf.PT_LOAD
                 then ( if !trace_elf
                        then ( print ( "Loading segment ...\n")
                             ; Elf.printPSeg s
                             )
                        else ()
                      ; storeVecInMem ((#vaddr s), (#memsz s), (#bytes s))
                      (* TODO: should check flags for executable segment *)
                      ; if dis then disassemble (#vaddr s) (#memsz s)
                        else ()
                      )
                 else ( print ("Skipping ")
                      ; Elf.printPSeg s
                      )
             ) segs

fun initPlatform (cores) =
    ( riscv.print     := debugPrint
    ; riscv.println   := debugPrintln
    ; riscv.procID    := BitsN.B(0, BitsN.size(!riscv.procID))
    ; riscv.totalCore := cores
    ; riscv.initMem (BitsN.fromInt
                         ((if !verify then 0xaaaaaaaaAAAAAAAA else 0x0)
                         , 64))
    ; if !verify
      then initVerify ()
      else ()
    )

(* assumes riscv.procID is 0 *)
fun initCores (arch, pc) =
    ( riscv.initIdent arch
    ; riscv.initMachine (!riscv.procID)
    ; riscv.initRegs pc
    ; if isLastCore ()
      then ()  (* core scheduler will wrap back to first core *)
      else ( riscv.scheduleCore (nextCoreToSchedule ())
           ; initCores (arch, pc)
           )
    )

fun doElf cycles file dis =
    let val elf   = Elf.openElf file
        val hdr   = Elf.getElfHeader elf
        val psegs = Elf.getElfProgSegments elf hdr
    in  initCores (if (#class hdr) = Elf.BIT_32
                   then riscv.RV32I else riscv.RV64I
                  , if !boot then 0x200 else (#entry hdr)
                  )
      ; if !trace_elf
        then ( print "Loading elf file ...\n"
             ; Elf.printElfHeader hdr
             )
        else ()
      ; be := (if (#endian hdr = Elf.BIG) then true else false)
      ; loadElf psegs dis
      ; if !verify
        then loadVerify file
        else ()

      ; if dis
        then logLevels (0)
        else runWrapped cycles
    end

(* Command line interface *)

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
            | "-v"   => "--verify"
            | "-m"   => "--multi"
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
            val (t, l) = processOption "--trace"  l
            val (d, l) = processOption "--dis"    l
            val (v, l) = processOption "--verify" l
            val (m, l) = processOption "--multi"  l

            val c = Option.getOpt (Option.map getNumber c, ~1)
            val d = Option.getOpt (Option.map getBool d, false)
            val t = Option.getOpt (Option.map getNumber t, !trace_level)
            val m = Option.getOpt (Option.map getNumber m, 1)
            val () = trace_level := Int.max (0, t)
            val v = Option.getOpt (Option.map getBool v, !verify)
            val () = verify := v
        in
            if List.null l then printUsage ()
            else ( initPlatform (m)
                 ; doElf c (List.hd l) d
                 )
        end
end
