(* Copyright (C) 2014, 2015 Anthony Fox, University of Cambridge
 * Copyright (C) 2014, 2015 Alexandre Joannou, University of Cambridge
 * Copyright (C) 2015-2018  SRI International.
 *
 * This software was developed by SRI International and the University of
 * Cambridge Computer Laboratory under DARPA/AFRL contract FA8750-11-C-0249
 * ("MRC2"), as part of the DARPA MRC research programme, and under
 * DARPA/AFRL contract FA8750-10-C-0237 ("CTSRD"), as part of the DARPA
 * CRASH research programme, and under DARPA/AFRL contract FA8650-18-C-7809
 * ("CIFV").
 *
 * See the LICENSE file for details.
 *)

(* --------------------------------------------------------------------------
   RISCV emulator
   -------------------------------------------------------------------------- *)

(* Default Configuration *)

val mem_base_addr   = ref (IntInf.fromInt 0x80000000) (* default used in spike *)
val mem_size        = ref (IntInf.fromInt 0)

(* Execution parameters *)

(* true  -> init starting PC to reset vector
   false -> use start offset from ELF *)
val boot        = ref false
val reset_addr  = 0x1000  (* default used in spike *)

val be          = ref false (* little-endian *)
val time_run    = ref true

val trace_lvl   = ref (0 : int)
val trace_elf   = ref false

val check           = ref false
val checker         = ref (NONE : Oracle.t option)
val checker_exit_pc = ref (Word64.fromInt (~1))

val verifier_mode       = ref false
val verifier_exe_name   = "SIM_ELF_FILENAME"
val verifier_trace_lvl  = ref 1

(* Utilities *)

fun hex s  = L3.lowercase (BitsN.toHexString s)
fun phex n = StringCvt.padLeft #"0" (n div 4) o hex
val hex16  = phex 16
val hex32  = phex 32
val hex64  = phex 64

fun hx32  n = Word32.fmt StringCvt.HEX n
fun hx64  n = Word64.fmt StringCvt.HEX n
fun hxi   n = Int.fmt    StringCvt.HEX n
fun hxi64 n = IntInf.fmt StringCvt.HEX n

fun failExit s = ( print (s ^ "\n"); OS.Process.exit OS.Process.failure )
fun err e s = failExit ("Failed to " ^ e ^ " file \"" ^ s ^ "\"")

fun debugPrint s = print("==DEBUG== "^s)
fun debugPrintln s = print("==DEBUG== "^s^"\n")

fun mkMask64 w =
    Word64.-(Word64.<<(Word64.fromInt 0x1,
                       Word.fromInt (IntInf.toInt (BitsN.toUInt w))),
             Word64.fromInt 0x1)

(* Bit vector utilities *)

fun word8ToBits8 word8 =
    BitsN.fromInt (Word8.toLargeInt word8, 8)

fun getByte v i =
    if   i < Word8Vector.length v
    then word8ToBits8 (Word8Vector.sub (v, i))
    else BitsN.zero 8

(* Memory utilities *)

(* TODO: this might be broken for big-endian code, but RISCV is
   little-endian by default. *)
fun storeVecInMemHelper vec (base : int) (i : int) =
    let val j = 8*i;
        val bytes0  = List.tabulate (8, fn inner => getByte vec (j+inner));
        val bytes1  = if !be then bytes0 else rev bytes0
        val bits64  = BitsN.concat bytes1
        val addr    = IntInf.fromInt (base + j)
        val vlen    = Word8Vector.length vec
    in  if   j < vlen
        then ( riscv.extRawWriteMem (BitsN.fromInt (addr, IntInf.fromInt 64), bits64)
             ; storeVecInMemHelper vec base (i+1)
             )
        else
            if   !trace_elf
            then print (Int.toString (Word8Vector.length vec) ^ " words.\n")
            else ()
    end

fun storeVecInMem (base : int, memsz : int, vec) =
    let val vlen   = Word8Vector.length vec
        val padded = if   memsz <= vlen then vec
                     else (
                         let val pad = Word8Vector.tabulate
                                           (memsz - vlen,  (fn _ => Word8.fromInt 0))
                         in  Word8Vector.concat (vec :: pad :: [])
                         end
                     )
    in  storeVecInMemHelper padded base 0
    end

(* Physical memory model: match Spike for now, but needs to be configurable. *)

val RSTVEC_BASE = 0x1000
val RSTVEC_SIZE = 0x20
val DRAM_BASE   = 0x80000000
val DRAM_SIZE   = 2048 * 1024 * 1024

val physMemRanges : (IntInf.int * IntInf.int) list ref =
    ref [ (RSTVEC_BASE, RSTVEC_SIZE)
        , (DRAM_BASE,   DRAM_SIZE)
        ]

fun isMemoryAddr intAddr =
    List.exists (fn (s, r) =>
                    s <= intAddr andalso intAddr < (s + r)
                ) (!physMemRanges)

(* Multi-core utilities *)

fun currentCore () =
    BitsN.toInt (!riscv.procID)

fun nextCoreToSchedule () =
    (1 + currentCore ()) mod !riscv.totalCore

fun isLastCore () =
    BitsN.toInt (!riscv.procID) + 1 = !riscv.totalCore

(* Printing utilities *)

fun printLog () =
    ( List.app (fn (n, l) =>
                   if   IntInf.toInt n <= !trace_lvl
                   then print (l ^ "\n")
                   else ()
               ) (List.rev (!riscv.log))
    ; riscv.clear_logs ()
    )

local
    fun readReg i = hex64 (riscv.GPR (BitsN.fromNat (i, 5)))
in
fun dumpRegisters core =
    let val savedCore   = currentCore ()
        val pc          = riscv.Map.lookup(!riscv.c_PC, core)
    in  riscv.scheduleCore core
      ; printLog ()
      ; print "======   Registers   ======\n"
      ; print ("Core = " ^ IntInf.toString core
               ^ " at privilege " ^ riscv.privName (riscv.curPrivilege ())
               ^ "\n")
      ; let val w   = #instr (riscv.Delta ())
            val i   = riscv.Decode w
        in  print ("Faulting instruction: (0x" ^ hex32 w ^ ") "
                   ^ riscv.instructionToString i
                   ^ "\n\n")
        end

      ; print ("PC     " ^ hex64 pc ^ "\n")
      ; L3.for
            (IntInf.fromInt 0, IntInf.fromInt 31,
             fn i =>
                print ("reg " ^ (if IntInf.< (i, 10) then " " else "") ^
                       IntInf.toString i ^ " " ^ readReg i ^ "\n"))
      ; riscv.scheduleCore savedCore
    end
end

local
    fun disRVC arch addr ilo =
        let val inst = riscv.DecodeRVC (arch, ilo)
        in  print ("0x" ^ (L3.padLeftString(#"0", (10, BitsN.toHexString addr)))
                   ^ ": 0x" ^ hex16 ilo
                   ^ ":     " ^ riscv.instructionToString inst
                   ^ "\n"
                  )
        end
    fun disBase addr ilo ihi =
        let val word = BitsN.concat [ihi, ilo]
            val inst = riscv.Decode word
        in  print ("0x" ^ (L3.padLeftString(#"0", (10, BitsN.toHexString addr)))
                   ^ ": 0x" ^ hex32 word
                   ^ ": " ^ riscv.instructionToString inst
                   ^ "\n"
                  )
        end
in
fun disassemble arch pc range =
    if range <= 0 then ()
    else let val addr   = BitsN.fromInt (pc, IntInf.fromInt 64)
             val ilo    = riscv.rawReadInstGranule (addr)
             val is_rvc = riscv.isRVC ilo
         in  if   is_rvc
             then ( disRVC arch addr ilo
                  ; disassemble arch (pc + 2) (range - 2)
                  )
             else ( let val pc_plus_2 = BitsN.+(addr, BitsN.fromInt(2, 64))
                    in  disBase addr ilo (riscv.rawReadInstGranule pc_plus_2)
                      ; disassemble arch (pc + 4) (range - 4)
                    end
                  )
         end
end

fun verifierTrace (lvl, str) =
    if   lvl <= !verifier_trace_lvl
    then print (String.concat ["L3RISCV: ", str, "\n"])
    else ()

(* Tandem verification:
   client interface: we are a client of an external oracle. *)

fun setChecker hndl =
    checker := SOME hndl

fun getChecker () =
    case !checker of
        NONE   => failExit "Verification oracle not initialized!"
     |  SOME h => h

local
    fun check_pc  t pc =
        Oracle.checkPC (t, BitsN.toUInt pc)
    fun check_reg t rdb rvb =
        let val (rd, rv) = (IntInf.toInt (BitsN.toUInt rdb), BitsN.toUInt rvb)
        in  Oracle.checkGPR (t, rd, rv)  end
    fun check_csr_int t rn rvb =
        let val rv = BitsN.toUInt rvb
        in  Oracle.checkCSR (t, rn, rv)  end
    fun check_csr t rnb rvb =
        let val rn = IntInf.toInt (BitsN.toUInt rnb)
        in  check_csr_int t rn rvb  end

    fun check_mstatus t rvb = check_csr_int t 0x300 rvb
    fun check_misa    t rvb = check_csr_int t 0x301 rvb
    fun check_mepc    t rvb = check_csr_int t 0x341 rvb
    fun check_mcause  t rvb = check_csr_int t 0x342 rvb
    fun check_mtval   t rvb = check_csr_int t 0x343 rvb
    fun check_sepc    t rvb = check_csr_int t 0x141 rvb
    fun check_scause  t rvb = check_csr_int t 0x142 rvb
    fun check_stval   t rvb = check_csr_int t 0x143 rvb

    fun check_failed msg (chks, vals) =
        ( ListPair.app (fn (c, v) => if   v then ()
                                     else print (" mis-matched " ^ c ^ "\n")
                       ) (chks, vals)
        ; if   List.exists (fn b => not b) vals
          then ( print (msg ^ " verification error.\n")
               ; dumpRegisters (currentCore ())
               ; failExit "Verification FAILED!\n"
               )
          else ()
        )
in

fun doInitCheck () =
    let val t       = getChecker ()
        val priv_ok = Oracle.checkPriv (t, riscv.curPrivilege ())
        val pc_ok   = Oracle.checkPC (t, BitsN.toUInt (riscv.PC ()))
        val isa_ok  = check_misa    t (riscv.reg'misa (#misa (riscv.MCSR ())))
        val ms_ok   = check_mstatus t (riscv.reg'mstatus (#mstatus (riscv.MCSR ())))
        val regs_ok = ref []
    in  check_failed "Initialization" ( ["privilege", "pc", "misa", "mstatus"]
                                      , [priv_ok, pc_ok, isa_ok, ms_ok]
                                      )
      ; L3.for
            (IntInf.fromInt 0, IntInf.fromInt 31,
             fn i =>
                let val rd = BitsN.fromNat (i, 5)
                    val rv = riscv.GPR rd
                in regs_ok := (check_reg t rd rv) :: !regs_ok
                end
            )
      ; check_failed "Initialization" ( (List.map (fn _ => "reg") (!regs_ok))
                                      , !regs_ok
                                      )
      ; print ("Initialization state passed checks.\n")
    end
fun doCheck () =
    let val delta   = riscv.Delta ()
        val t       = getChecker ()
        val priv_ok = Oracle.checkPriv (t, (#priv delta))
        val pc_ok   = Oracle.checkPC (t, BitsN.toUInt (#pc delta))
        (* TODO: inst check *)
        val reg_ok  = case #reg_effect delta of
                          NONE          => true
                       |  SOME (rd, rv) => check_reg t rd rv
        val csr_ok  = case #csr_effect delta of
                          NONE          => true
                       |  SOME (rd, rv) => check_csr t rd rv
        val ms_ok   = check_mstatus t (#mstatus delta)

        val mpc_ok  = case #mepc delta of
                          NONE          => true
                       |  SOME rv       => check_mepc t rv
        val mc_ok   = case #mcause delta of
                          NONE          => true
                       |  SOME rv       => check_mcause t rv
        val mtv_ok  = case #mtval delta of
                          NONE          => true
                       |  SOME rv       => check_mtval t rv

        val spc_ok  = case #sepc delta of
                          NONE          => true
                       |  SOME rv       => check_sepc t rv
        val sc_ok   = case #scause delta of
                          NONE          => true
                       |  SOME rv       => check_scause t rv
        val stv_ok  = case #stval delta of
                          NONE          => true
                       |  SOME rv       => check_stval t rv

    in  check_failed "State" ( [ "privilege", "pc", "reg", "csr", "mstatus"
                                 , "mepc", "mcause", "mtval"
                                 , "sepc", "scause", "stval"
                               ]
                             , [ priv_ok, pc_ok, reg_ok, csr_ok, ms_ok
                                 , mpc_ok, mc_ok, mtv_ok
                                 , spc_ok, sc_ok, stv_ok
                               ]
                             )
    end
end

fun isCheckerDone () =
    Oracle.isDone (getChecker ())

(* Code execution *)

fun logLoop mx i =
    ( riscv.scheduleCore (nextCoreToSchedule ())
    ; riscv.Next ()
    ; print ("\n")
    ; printLog ()
    ; if   !check
      then ( Oracle.step (getChecker ())
           ; doCheck()
           )
      else ()
    ; if   !riscv.done orelse i = mx orelse (!check andalso isCheckerDone ())
      then ( print ("ExitCode: " ^ Nat.toString (riscv.exitCode ()) ^ "\n")
           ; print ("Completed " ^ Int.toString (i + 1) ^ " instructions.\n")
           )
      else logLoop mx (if isLastCore () then (i + 1) else i)
    )

local
    fun t f x = if !time_run then Runtime.time f x else f x
in
fun run mx =
    ( t (logLoop mx) 0
    ; if   !riscv.done
      then Unix.exit (Word8.fromInt (IntInf.toInt (Nat.toInt (riscv.exitCode ()))))
      else ()
    )

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

(* Platform initialization *)

fun physAddrIsMemory (a : BitsN.nbit, n : IntInf.int) =
    let val addr = BitsN.toInt a
        val nend = IntInf.-(n, 1)
    in  isMemoryAddr addr andalso isMemoryAddr (IntInf.+(addr, nend))
    end

fun initPlatform cores =
    ( riscv.print     := debugPrint
    ; riscv.println   := debugPrintln
    ; riscv.procID    := BitsN.B(0, BitsN.size(!riscv.procID))
    ; riscv.totalCore := cores
    ; riscv.initMem (BitsN.fromInt
                         ((if !check then 0xaaaaaaaaAAAAAAAA else 0x0)
                         , 64))
    ; riscv.validMemAddrPred    := physAddrIsMemory
    ; riscv.enable_dirty_update := true
    ; if   !check
      then setChecker (Oracle.init ("RV64IMAFDC"))
      else ()
    )

(* assumes riscv.procID is 0 *)
fun initCores (arch, pc) =
    ( riscv.initIdent arch
    ; riscv.initMachine (!riscv.procID)
    ; riscv.initRegs pc
    ; if   isLastCore ()
      then ()  (* core scheduler will wrap back to first core *)
      else ( riscv.scheduleCore (nextCoreToSchedule ())
           ; initCores (arch, pc)
           )
    )

(* Spike-compatible reset vector *)

fun insertResetVec pc =
    (* reset vector from Spike, sim.cc:make_dtb() *)
    let val rst_vec_sz  = 8 * 4
        val auipc_imm   = BitsN.B(0x0, 20)
        val rvsz_imm    = BitsN.fromInt (rst_vec_sz, 12)
        val mhartid_imm = BitsN.B((0x0F14: IntInf.int), 12)
        val boot_code   =
            [ (* auipc  t0, 0x0 *)
              riscv.ArithI(riscv.AUIPC(BitsN.fromNat (5, 5), auipc_imm))
            , (* addi   a1, t0, reset_vec_size *)
              riscv.ArithI(riscv.ADDI(BitsN.fromNat (11, 5), (BitsN.fromInt (5, 5), rvsz_imm)))
            , (* csrr   a0, mhartid *)
              riscv.System(riscv.CSRRS(BitsN.fromNat (10, 5), (BitsN.fromNat (0, 5), mhartid_imm)))
            , (* ld     t0, 24(t0)   TODO: xlen=32 => lw t0, 24(t0) *)
              riscv.Load(riscv.LD(BitsN.fromNat (5, 5), (BitsN.fromNat (5, 5), BitsN.fromNat (24, 12))))
            , (* jr     t0 *)
              riscv.Branch(riscv.JALR(BitsN.fromNat (0, 5), (BitsN.fromNat (5, 5), BitsN.zero 12)))
            ]
        val pc_int       = IntInf.fromInt pc
        val pc_lo        = IntInf.andb (pc_int, 0xFFFFFFFF)
        val pc_hi        = IntInf.~>> (pc_int, Word.fromInt 32)
        fun insert (addr : IntInf.int) words =
            case words of
                w :: tl        => ( riscv.rawWriteMem (BitsN.fromInt (addr, 64), (w, 4))
                                  ; insert (IntInf.+ (addr, 4)) tl
                                  )
              | []             => addr
    in print ("L3RISCV: Loading reset code at " ^ hxi reset_addr ^ "\n")
     ; let val pc_addr =
               insert (IntInf.fromInt reset_addr) (List.map riscv.Encode boot_code)
       in printLog ()
        (* the reset-vector terminates with a 64-bit aligned start-address. *)
        ; insert pc_addr [ BitsN.fromInt (  0x0, 32)  (* align *)
                         , BitsN.fromInt (pc_lo, 32)
                         , BitsN.fromInt (pc_hi, 32)
                         ]
        ; printLog ()
       end
    end


(* Program load *)

fun loadElf arch segms dis =
    List.app (fn s =>
                 if   (#ptype s) = Elf.PT_LOAD
                 then (let val vaddr   = Int.fromLarge (#vaddr s)
                           val memsz   = Int.fromLarge (#memsz s)
                           val mem_end = IntInf.toInt (IntInf.+ (!mem_base_addr, !mem_size))
                       in
                           if   !trace_elf
                           then ( print ("Loading segment @ vaddr=" ^ hxi vaddr
                                          ^ ", " ^ Int.toString memsz ^ " bytes ...\n")
                                ; Elf.printSegment s
                                )
                           else ()
                         ; storeVecInMem (vaddr, memsz, (#bytes s))
                         (* update memory range *)
                         ; if   vaddr < IntInf.toInt (!mem_base_addr)
                           then mem_base_addr := IntInf.fromInt vaddr
                           else ()
                         ; if   vaddr + memsz > mem_end
                           then mem_size := IntInf.fromInt (vaddr + memsz - mem_end)
                           else ()
                         (* TODO: should check flags for executable segment *)
                         ; if   dis then disassemble arch (#vaddr s) (#memsz s)
                           else ()
                       end)
                 else ( print ("Skipping segment ...\n")
                      ; Elf.printSegment s
                      )
             ) segms

fun match_symb (name : string) (s : Elf.symb) =
    case (#syname s) of
        NONE    => false
     |  SOME nm => Substring.string nm = name

fun set_tohost (tohost : Elf.symb option) =
    case tohost of
        NONE   =>
        print "L3RISCV: no tohost symbol found!\n"
     |  SOME s =>
        let val addr = Int.fromLarge (#syvalue s)
        in print ("L3RISCV: tohost mapped to 0x" ^ (hxi addr) ^ "\n")
         ; riscv.htif_tohost_addr := BitsN.fromInt(IntInf.fromInt addr, IntInf.fromInt 64)
        end

fun setupElf file dis =
    let val elf    = Elf.openElf file
        val hdr    = Elf.getHeader elf
        val segms  = Elf.getSegments elf hdr
        val sects  = Elf.getSections elf hdr
        val nsects = Elf.getNamedSections elf hdr sects
        val symbs  = Elf.getSymbols elf hdr nsects
        val pc     = if !boot then reset_addr else (LargeInt.toInt (#entry hdr))
        val tohost = List.find (match_symb "tohost") symbs
        val arch   = if (#class hdr) = Elf.BIT_32 then riscv.RV32 else riscv.RV64
    in  set_tohost tohost
      (* FIXME: instead of presenting as RV64 in misa/mstatus for RV32 ELF
       * files, we should support presenting as RV32 using writable xXLs.  This
       * should be made to support tandem-verification with spike.
       *)
      ; initCores ( riscv.RV64 (* TODO: use arch instead? *)
                  , IntInf.fromInt pc
                  )
      ; print ("L3RISCV: pc set to 0x" ^ (hx64 (Word64.fromInt pc))
               ^ (if !boot then " [boot-entry] " else " [elf-entry]")
               ^ " in file " ^ file ^ "\n")
      ; if   !trace_elf
        then ( print "Loading elf file ...\n"
             ; Elf.printHeader hdr
             ; List.app Elf.printNamedSection nsects
             ; List.app Elf.printSymbol symbs
             )
        else ()
      ; be := (if (#endian hdr = Elf.BIG) then true else false)
      ; loadElf arch segms dis
      ; if   !trace_elf
        then ( print ("\nMem base: " ^ (hxi64 (!mem_base_addr)))
             ; print ("\nMem size: " ^ (hxi64 (!mem_size))
                      ^ " (" ^ (IntInf.fmt StringCvt.DEC (!mem_size)) ^ ")\n")
             )
        else ()
      ; printLog ()
      (* FIXME: this spike-specific behaviour of loading a reset-vector in a debug-rom
       * should be conditioned by an appropriate flag. *)
      ; if   !boot
        then insertResetVec (LargeInt.toInt (#entry hdr))
        else ()
    end

fun doElf cycles file dis =
    ( setupElf file dis
    ; if   !check
      then ( Oracle.loadElf (getChecker (), file)
           ; Oracle.reset (getChecker ())
           ; doInitCheck ()
           )
      else ()
    ; if   dis
      then printLog ()
      else runWrapped cycles
    )

(* Tandem verification:
   server interface: verify against model *)

(* TODO *)
fun initModel () =
    ()

(* Command line interface *)

fun printUsage () =
    print
        ("\nRISCV emulator (based on an L3 specification).\n\
          \http://www.cl.cam.ac.uk/~acjf3/l3\n\n\
          \usage: " ^ OS.Path.file (CommandLine.name ()) ^ " [arguments] file\n\n\
          \Arguments:\n\
          \  --dis    <bool>      only disassemble loaded code\n\
          \  --cycles <number>    upper bound on instruction cycles\n\
          \  --trace  <level>     verbosity level (0 default, 4 maximum)\n\
          \  --multi  <#cores>    number of cores (1 default)\n\
          \  --check  <bool>      check execution against external verifier\n\
          \  --boot   <bool>      set starting pc to reset address x1000 (false default)\n\
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
        | "-k"   => "--check"
        | "-m"   => "--multi"
        | "-v"   => "--verifier"
        | "-b"   => "--boot"
        | s      => s
        ) (CommandLine.arguments ())

fun processOption (s: string) =
    let fun loop acc =
            fn a :: b :: r =>
               if   a = s
               then (SOME b, List.rev acc @ r)
               else loop (a :: acc) (b :: r)
          | r => (NONE, List.rev acc @ r)
    in  loop []
    end

fun main_wrapped () =
    case getArguments () of
        ["--help"] => printUsage ()
      | l =>
        let val (c, l) = processOption "--cycles"   l
            val (t, l) = processOption "--trace"    l
            val (d, l) = processOption "--dis"      l
            val (k, l) = processOption "--check"    l
            val (m, l) = processOption "--multi"    l
            val (v, l) = processOption "--verifier" l
            val (b, l) = processOption "--boot"     l

            val c = Option.getOpt (Option.map getNumber c, ~1)
            val d = Option.getOpt (Option.map getBool d, !trace_elf)
            val t = Option.getOpt (Option.map getNumber t,
                                   (IntInf.fromInt (!trace_lvl)))
            val m = Option.getOpt (Option.map getNumber m, 1)
            val k = Option.getOpt (Option.map getBool k, !check)
            val v = Option.getOpt (Option.map getBool v, !verifier_mode)
            val b = Option.getOpt (Option.map getBool b, !boot)

            val () = trace_lvl      := Int.max (0, IntInf.toInt t)
            val () = check          := k
            val () = trace_elf      := d
            val () = verifier_mode  := v
            val () = boot           := b

        in  if   List.null l andalso not (!verifier_mode)
            then printUsage ()
            else ( initPlatform (m)
                 ; if   !verifier_mode
                   then initModel ()
                   else doElf (IntInf.toInt c) (List.hd l) d
                 )
        end

fun main () =
    main_wrapped ()
    handle e => print ("Exception error:" ^ (exnMessage e) ^ "\n")
