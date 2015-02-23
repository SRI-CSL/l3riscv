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
val nb_core = ref 1

(* --------------------------------------------------------------------------
   Loading code into memory from Hex file
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

(*
fun printHex a =
    let val problematic = ref ([] : BitsN.nbit list)
    in  Array.appi
            (fn (i, w) =>
                let val s = riscv.instructionToString (riscv.Decode w)
                    val () = if s = "???" then problematic := w :: !problematic
                             else ()
                in   print (hex32 w ^ " " ^ s ^ "\n")
                end) a
      ; L3.mkSet (!problematic)
    end
*)

fun storeArrayInMem (base, marray) =
   let val b = IntInf.andb (base div 8, 0x1fffffffff)
   in  L3.for (0, Array.length marray div 2 - 1,
               fn i =>
                  let val w1 = Array.sub (marray, 2 * i)
                      val w2 = Array.sub (marray, 2 * i + 1)
                      val dw = BitsN.@@ (w1, w2)
                  in  riscv.writeMem(BitsN.fromInt(b + i, 37), dw)
                  end
                  handle Subscript => print (Int.toString i ^ "\n"))
     ; print (Int.toString (Array.length marray) ^ " words.\n")
   end

(* --------------------------------------------------------------------------
   Loading code into memory from raw file
   -------------------------------------------------------------------------- *)

fun word8ToBits8 word8 =
    BitsN.B (Word8.toInt word8, 8)

fun getByte v i =
    if   i < Word8Vector.length v
    then word8ToBits8 (Word8Vector.sub (v, i))
    else BitsN.zero 8

fun storeVecInMemHelper vec base i =
    let val j = 8*i;
        val bytes0  = List.tabulate (8, fn inner => getByte vec (j+inner));
        val bytes1  = if !be then bytes0 else rev bytes0
        val bits64  = BitsN.concat bytes1
    in  if   j < Word8Vector.length vec
        then ( riscv.writeMem(BitsN.fromInt(base+i, 35), bits64)
             ; print (Int.toString(base+i) ^ ": 0x" ^ BitsN.toHexString(bits64)^"\n")
             ; storeVecInMemHelper vec base (i+1)
             )
        else print (Int.toString (Word8Vector.length vec) ^ " words.\n")
    end

fun storeVecInMem (base, vec) =
    let val b = IntInf.andb (base div 8, 0x7ffffffff)
    in  storeVecInMemHelper vec b 0
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

(* ------------------------------------------------------------------------
   Run code
   ------------------------------------------------------------------------ *)

fun loop mx i =
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
        else loop mx (i + 1)
    end

fun decr i = if i <= 0 then i else i - 1

fun pureLoop mx =
    ( riscv.procID := BitsN.B(!current_core_id,
                              BitsN.size(!riscv.procID))
    ; riscv.Next ()
    ; printLog(0)
    ; if !riscv.done orelse (mx = 1) then (print "done\n")
      else pureLoop (decr mx)
    )

local
    fun t f x = if !time_run then Runtime.time f x else f x
in
fun run_mem mx =
    if 1 <= !trace_level then t (loop mx) 0 else t pureLoop mx

fun run pc mx psegs =
    ( riscv.procID := BitsN.B(!current_core_id, BitsN.size(!riscv.procID))
    ; riscv.initRISCV pc
    ; riscv.totalCore := 1
    ; riscv.print := debug_print
    ; riscv.println := debug_println
    ; List.app
          (fn seg =>
              if (#ptype seg) = Elf.PT_LOAD
              then ( print ("Loading ")
                   ; Elf.printPSeg seg
                   ; storeVecInMem ((#vaddr seg), (#bytes seg))
                   )
              else ( print ("Skipping ")
                   ; Elf.printPSeg seg
                   )
          ) psegs
    ; run_mem mx
    )
    handle riscv.UNPREDICTABLE s =>
           ( List.tabulate(!nb_core, dumpRegisters)
           ; failExit ("UNPREDICTABLE \"" ^ s ^ "\"\n")
           )
end

fun runElf cycles file =
    let val elf   = Elf.openElf file
        val hdr   = Elf.getElfHeader elf
        val psegs = Elf.getElfProgSegments elf hdr
    in  be := (if (#endian hdr = Elf.BIG) then true else false)
      ; run (#entry hdr) cycles psegs
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
              \  --cycles <number>    upper bound on instruction cycles\n\
              \  --trace <level>      verbosity level (0 default, 2 maximum)\n\
              \  -h or --help         print this message\n\n")

    fun getNumber s =
        case IntExtra.fromString s of
            SOME n => n
          | NONE   => failExit ("Bad number: " ^ s)

    fun getArguments () =
        List.map
            (fn "-c" => "--cycles"
            | "-t"   => "--trace"
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
        in
            if List.null l then printUsage ()
            else runElf c (List.hd l)
        end
end
