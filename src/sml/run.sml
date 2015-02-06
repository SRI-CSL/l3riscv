(* --------------------------------------------------------------------------
   MIPS emulator (based on SML generated from an L3 specification)
   -------------------------------------------------------------------------- *)

(* --------------------------------------------------------------------------
   Default Configuration
   -------------------------------------------------------------------------- *)

val be = ref true (* big-endian *)
val trace_level = ref 0
val time_run = ref true
val uart_delay = ref 5000
val uart_countdown = ref (!uart_delay)
val uart_in = ref (NONE: TextIO.instream option)
val uart_out = ref (NONE: TextIO.outstream option)
val non_blocking_input = ref false
val current_core_id = ref 0
val nb_core = ref 1
val schedule = ref (NONE: TextIO.instream option)

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

local
   val endLine = ":00000001FF"
   fun inc16 a = BitsN.+ (a, BitsN.fromInt (1, 16))
   fun checkEnd s r = if s = endLine then r else raise Fail ("Bad line: " ^ s)
   fun readIntelHex s =
      if String.substring (s, 0, 3) <> ":04" orelse
         String.substring (s, 7, 2) <> "00" then NONE
      else let
              val a = w16 (String.substring (s, 3, 4))
              val b0 = w8 (String.substring (s, 9, 2))
              val b1 = w8 (String.substring (s, 11, 2))
              val b2 = w8 (String.substring (s, 13, 2))
              val b3 = w8 (String.substring (s, 15, 2))
              val l = [b3, b2, b1, b0]
           in
              SOME (a, BitsN.concat (if !be then l else List.rev l))
           end
   fun toArray l =
      Array.fromList
        (if List.length l mod 2 = 0 then l else l @ [BitsN.zero 32])
in
   fun loadIntelHex s =
      let
         val strm = TextIO.openIn s
         val inp = TextIO.inputAll strm before TextIO.closeIn strm
         val l = String.tokens (fn c => c = #"\n") inp
      in
         case l of
            [] => raise Fail ("File empty: " ^ s)
          | h :: t =>
              case OS.Path.ext s of
                 SOME "phex" => (toArray (List.map w32 l)
                                 handle Option.Option => err "parse" s)
               | _ =>
                 (case readIntelHex h of
                     SOME (a0, w0) =>
                       let
                          val () = if a0 <> BitsN.fromInt (0, 16)
                                      then raise Fail
                                             ("Does not start at zero: " ^ s)
                                   else ()
                          val (_, r) =
                             List.foldl
                                (fn (s, (pa, l)) =>
                                   case readIntelHex s of
                                      SOME (a, w) =>
                                         if inc16 pa = a
                                            then (a, w :: l)
                                         else ( print (BitsN.toHexString pa)
                                              ; print "\n"
                                              ; print (BitsN.toHexString a)
                                              ; raise Fail
                                                  ("Not sequential: " ^ s)
                                              )
                                    | NONE => checkEnd s (pa, l))
                                (a0, [w0]) t
                       in
                          toArray (List.rev r)
                       end
                   | NONE => checkEnd h (Array.fromList []))
      end
      handle IO.Io {function  = "TextIO.openIn", ...} => err "open" s
end

(*
fun printHex a =
   let
      val problematic = ref ([] : BitsN.nbit list)
   in
      Array.appi
        (fn (i, w) =>
           let
              val s = mips.instructionToString (mips.Decode w)
              val () = if s = "???" then problematic := w :: !problematic
                       else ()
           in
              print (hex32 w ^ " " ^ s ^ "\n")
           end) a
    ; L3.mkSet (!problematic)
   end
*)

fun storeArrayInMem (base, marray) =
   let
      val b = IntInf.andb (base div 8, 0x1fffffffff)
   in
      L3.for (0, Array.length marray div 2 - 1,
        fn i =>
           let
              val w1 = Array.sub (marray, 2 * i)
              val w2 = Array.sub (marray, 2 * i + 1)
              val dw = BitsN.@@ (w1, w2)
           in
              mips.WriteDWORD(BitsN.fromInt(b + i, 37), dw)
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
  let
    val j = 32*i;
    val bytes0  = List.tabulate(4, fn outer => List.tabulate (8, fn inner => getByte vec (j+outer*8+inner)));
    val bytes1  = if !be then bytes0 else map rev bytes0
    val bits256 = BitsN.concat (rev (map BitsN.concat bytes1))
  in
    if   j < Word8Vector.length vec
    then ( mips.Write256(BitsN.fromInt(base+i, 35), bits256)
         (*; print (Int.toString(base+i)^": 0x"^BitsN.toHexString(bits256)^"\n")*)
         ; storeVecInMemHelper vec base (i+1)
         )
    else print (Int.toString (Word8Vector.length vec) ^ " words.\n")
  end

fun storeVecInMem (base, vec) = 
  let
    val b = IntInf.andb (base div 32, 0x7ffffffff)
  in
    storeVecInMemHelper vec b 0
  end

fun readRaw filename =
  let
    val istream = BinIO.openIn filename
    val vec     = BinIO.inputAll istream
    val ()      = BinIO.closeIn istream
  in
    vec
  end

fun printLog (n) = List.app (fn e => print(e ^ "\n"))
                            (List.rev(mips.Map.lookup(!mips.log, n)))

local
   fun readReg i = hex64 (mips.GPR (BitsN.fromNat (i, 5)))
in
   fun dumpRegisters (core) =
     let
       val savedProcID = !mips.procID
       val pc = mips.Map.lookup(!mips.c_PC, 0)
       val () = mips.procID := BitsN.B(core, BitsN.size (!mips.procID))
     in
        print "======   Registers   ======\n"
      ; print ("Core = " ^ Int.toString(core) ^ "\n")
      ; print ("PC     " ^ hex64 pc ^ "\n")
      ; L3.for
          (0, 31,
           fn i =>
              print ("Reg " ^ (if i < 10 then " " else "") ^
                     Int.toString i ^ " " ^ readReg i ^ "\n"))
      ; mips.procID := savedProcID
    end
end

(* ------------------------------------------------------------------------
   UART I/O
   ------------------------------------------------------------------------ *)

val bytesToString = String.implode o List.map (Char.chr o BitsN.toNat)
val stringToBytes = List.map (fn c => BitsN.B (Char.ord c, 8)) o String.explode

fun uart_output () =
   let
      val s = bytesToString (mips.JTAG_UART_output ())
   in
      if s = ""
         then ()
      else case !uart_out of
              SOME strm => (TextIO.output (strm, s); TextIO.flushOut strm)
            | NONE => TextIO.print s
   end

fun uart_input () =
   let
      val n = 0xFFFF - BitsN.toNat (#RAVAIL (#data (!mips.JTAG_UART)))
   in
      if n = 0
         then ()
      else
        let
          val (readN, istrm) =
            case !uart_in of
                SOME strm => (n, strm)
              | NONE => if   !non_blocking_input
                        then case TextIO.canInput (TextIO.stdIn, n) of
                                 NONE   => (0, TextIO.stdIn)
                               | SOME x => (x, TextIO.stdIn)
                        else (print "UART in: "; (n, TextIO.stdIn))
        in
          if readN > 0 then
            mips.JTAG_UART_input (stringToBytes(TextIO.inputN(istrm,readN)))
          else ()
        end
   end


fun uart () =
   if !uart_delay <= 0 (* UART turned off *)
      then ()
   else if !uart_countdown <= 0
      then (uart_countdown := !uart_delay; uart_output (); uart_input ())
   else uart_countdown := !uart_countdown - 1

(* ------------------------------------------------------------------------
   Schedule (denoting order in which cores are interleaved)  
   ------------------------------------------------------------------------ *)

(* Returns next core id from given text stream *)
fun readNextCoreId stream =
  case TextIO.inputLine stream of
    NONE => failExit ("Reached end of schedule\n")
  | SOME line =>
        if line = "" then failExit ("Reached end of schedule\n")
        else 
          case IntExtra.fromString (String.extract(line, 0,
                 SOME (String.size(line)-1))) of
              NONE   => failExit ("Bad core id in schedule: " ^ line)
            | SOME i => if i >= !nb_core then (
                          failExit ("Core id in schedule >= nb_core: " ^ line)
                        ) else i

(* Returns id of next core to run *)
fun scheduleNext () = 
  case !schedule of
    NONE        => (!current_core_id+1) mod !nb_core
  | SOME stream => readNextCoreId stream

(* ------------------------------------------------------------------------
   Define UNPREDICTABLE "LO" and UNPREDICTABLE "HI" behaviour
   ------------------------------------------------------------------------ *)

val () = mips.UNPREDICTABLE_LO :=
  (fn _ => raise mips.UNPREDICTABLE "LO")

val () = mips.UNPREDICTABLE_HI :=
  (fn _ => raise mips.UNPREDICTABLE "HI")

(* ------------------------------------------------------------------------
   Run code
   ------------------------------------------------------------------------ *)

fun loop mx i =
   let
      val () = current_core_id := scheduleNext ()
      val () = mips.procID := BitsN.B(!current_core_id,
                                      BitsN.size(!mips.procID))
      val coreId = !current_core_id
      val exl0 = #EXL (#Status (mips.Map.lookup(!mips.c_CP0, coreId)))
      val pc = mips.Map.lookup(!mips.c_PC, coreId)
   in
    uart ()
    ; mips.instCnt := i
    ; mips.Next ()
    ; printLog(0)
    ; if 1 <= !trace_level then printLog(1) else ()
    ; if 2 <= !trace_level then printLog(2) else ()
    ; if !mips.done orelse i = mx
         then print ("Completed " ^ Int.toString (i + 1) ^ " instructions.\n")
      else 
        let val exl1 = #EXL (#Status (
                         mips.Map.lookup(!mips.c_CP0, coreId)))
        in loop mx (if not exl0 andalso exl1
                       then (print "exception\n"; i)
                    else i + 1)
        end
   end

fun decr i = if i <= 0 then i else i - 1

fun pureLoop mx =
   ( current_core_id := scheduleNext ()
   ; mips.procID := BitsN.B(!current_core_id,
                            BitsN.size(!mips.procID))
   ; uart ()
   ; mips.Next ()
   ; printLog(0)
   ; if !mips.done orelse (mx = 1) then (print "done\n")
     else pureLoop (decr mx)
   )

local
   fun t f x = if !time_run then Runtime.time f x else f x
in
   fun run_mem mx =
      if 1 <= !trace_level then t (loop mx) 0 else t pureLoop mx
   fun run pc_uart mx code raw =
      ( List.tabulate(!nb_core,
        fn x => (mips.procID := BitsN.B(x, BitsN.size(!mips.procID));
                 mips.initMips pc_uart))
      ; mips.totalCore := !nb_core
      ; mips.print := debug_print
      ; mips.println := debug_println
      ; List.app
          (fn (a, s) =>
             ( print ("Loading " ^ s ^ "... ")
             ; if   raw
               then storeVecInMem (a, readRaw s)
               else storeArrayInMem (a, loadIntelHex s)
             )) code 
      ; uart_countdown := !uart_delay
      ; if 0 < !uart_delay then uart_input () else ()
      ; run_mem mx
      ; if 0 < !uart_delay then uart_output () else ()
      )
      handle mips.UNPREDICTABLE s =>
        ( List.tabulate(!nb_core, dumpRegisters)
        ; failExit ("UNPREDICTABLE \"" ^ s ^ "\"\n")
        )
end

(* ------------------------------------------------------------------------
   Command line interface
   ------------------------------------------------------------------------ *)

fun printUsage () =
   print
    ("\nMIPS emulator (based on an L3 specification).\n\
      \http://www.cl.cam.ac.uk/~acjf3/l3\n\n\
      \usage: " ^ OS.Path.file (CommandLine.name ()) ^ " [arguments] file\n\n\
      \Arguments:\n\
      \  --cycles <number>          upper bound on instruction cycles\n\
      \  --trace <level>           verbosity level (0 default, 2 maximum)\n\
      \  --pc <address>            initial program counter value and\n\
      \                            start address for main Intel Hex file\n\
      \  --at <address> <file>     load extra Intel Hex <file> into physical\n\
      \                            memory at location <address>\n\
      \  --nbcore <number>         number of core(s) (default is 1)\n\
      \  --uart <address>          base physical address for UART memory-map\n\
      \  --uart-delay <number>     UART cycle delay (determines baud rate)\n\
      \  --uart-in <file>          UART input file (stdin if omitted)\n\
      \  --uart-out <file>         UART output file (stdout if omitted)\n\
      \  --format <format>         'raw' or 'hex' file format \n\
      \  --non-block <on|off>      non-blocking UART input 'on' or 'off' \n\
      \  --schedule <file>         file of core ids indicating schedule\n\
      \  --ignore <string>         UNPREDICTABLE#(<string>) behaviour is \n\
      \                            ignored (currently <string> must be \n\
      \                            'HI' or 'LO')                       \n\
      \  -h or --help              print this message\n\n")

fun getNumber s =
   case IntExtra.fromString s of
      SOME n => n
    | NONE => failExit ("Bad number: " ^ s)

fun getArguments () =
   List.map
      (fn "-c" => "--cycles"
        | "-t" => "--trace"
        | "-h" => "--help"
        | "-p" => "--pc"
        | "-a" => "--at"
        | "-n" => "--nbcore"
        | s => s)
      (CommandLine.arguments ())

fun processOption (s: string) =
   let
      fun loop acc =
         fn a :: b :: r =>
            if a = s
               then (SOME b, List.rev acc @ r)
            else loop (a :: acc) (b :: r)
          | r => (NONE, List.rev acc @ r)
   in
      loop []
   end

fun getCode p =
   let
      fun loop acc =
         fn "--at" :: a :: c :: r => loop ((getNumber a, c) :: acc) r
          | [r] => SOME (List.rev ((p, r) :: acc))
          | _ => NONE
   in
      loop []
   end

fun processOptions s commandLine =
  case processOption s commandLine of
      (NONE, rest)   => ([], rest)
    | (SOME x, rest) => let val (xs, rest2) = processOptions s rest
                        in  (x::xs, rest2)
                        end

val () =
   case getArguments () of
      ["--help"] => printUsage ()
    | l =>
       let
          val (fmt, l) = processOption "--format" l
          val raw = case fmt of
                        NONE       => false
                      | SOME "raw" => true
                      | SOME "hex" => false
                      | _          => failExit "--format must be raw or hex\n"
          val (igns, l) = processOptions "--ignore" l
          val () = List.app (fn x =>
                     case x of
                         "HI" => mips.UNPREDICTABLE_HI := (fn _ => ())
                       | "LO" => mips.UNPREDICTABLE_LO := (fn _ => ())
                       | _    => failExit "Unknown argument to --ignore"
                   ) igns
          val (nb, l) = processOption "--non-block" l
          val () = case nb of
                       NONE       => ()
                     | SOME "on"  => non_blocking_input := true
                     | SOME "off" => non_blocking_input := false
                     | _          => failExit "--non-block must be on or off\n"
          val (p, l) = processOption "--pc" l
          val p = Option.getOpt (Option.map getNumber p, 0x9000000040000000)
          val (u, l) = processOption "--uart" l
          val u = Option.getOpt (Option.map getNumber u, 0x000000007f000000)
          val (c, l) = processOption "--cycles" l
          val c = Option.getOpt (Option.map getNumber c, ~1)
          val (n, l) = processOption "--nbcore" l
          val () = nb_core := Option.getOpt (Option.map getNumber n, 1)
          val (t, l) = processOption "--trace" l
          val t = Option.getOpt (Option.map getNumber t, !trace_level)
          val () = trace_level := Int.max (0, t)
          val (d, l) = processOption "--uart-delay" l
          val () =
             uart_delay := Option.getOpt (Option.map getNumber d, !uart_delay)
          val (ui, l) = processOption "--uart-in" l
          val () = uart_in := Option.map TextIO.openIn ui
          val (sch, l) = processOption "--schedule" l
          val () = schedule := Option.map TextIO.openIn sch
          val (uo, l) = processOption "--uart-out" l
          val () = uart_out := Option.map TextIO.openOut uo
          fun closeStreams () =
            ( case !uart_in of
                 SOME stm => TextIO.closeIn stm
               | NONE => ()
            ; case !uart_out of
                 SOME stm => TextIO.closeOut stm
               | NONE => ()
            )
       in
          case getCode p l of
             SOME code => ((run (p, u) c code raw; closeStreams ())
                           handle e => (closeStreams (); raise e))
           | NONE => printUsage ()
       end
