---------------------------------------------------------------------------
--
-- RISC-V Model
-- Based on the MIPS specification by Anthony Fox, University of Cambridge
--
-- Copyright (C) 2014, 2015 Anthony Fox, University of Cambridge
-- Copyright (C) 2014, 2015 Alexandre Joannou, University of Cambridge
-- Copyright (C) 2015, SRI International.
--
-- This software was developed by SRI International and the University of
-- Cambridge Computer Laboratory under DARPA/AFRL contract FA8750-11-C-0249
-- ("MRC2"), as part of the DARPA MRC research programme.
--
-- See the LICENSE file for details.
--
---------------------------------------------------------------------------


---------------------------------------------------------------------------
-- Basic types
---------------------------------------------------------------------------

type id       = bits(8)           -- max 256 cores
type reg      = bits(5)
type creg     = bits(12)

type byte     = bits(8)
type half     = bits(16)
type word     = bits(32)
type dword    = bits(64)
type fpval    = bits(64)

type exc_code   = bits(5)
type priv_level = bits(2)

-- instruction fields
type opcode   = bits(7)
type imm12    = bits(12)
type imm20    = bits(20)

construct accessType { Read, Write }
construct fetchType  { Instruction, Data }

-- RV64* base.

type regType  = dword
type vAddr    = dword
type pAddr    = bits(61)        -- internal accesses are 8-byte aligned
type csrAR    = bits(2)         -- CSR access bits (from CSR address)

-- Miscellaneous
exception UNDEFINED :: string

---------------------------------------------------------------------------
-- Memory types for Load/Store instructions
---------------------------------------------------------------------------

type memWidth       = bits(3)

memWidth BYTE       = 0
memWidth HALFWORD   = 1
memWidth WORD       = 3
memWidth DOUBLEWORD = 7

---------------------------------------------------------------------------
-- Processor architecture
---------------------------------------------------------------------------

construct Architecture
{
  RV32Sv32, RV64Sv43
}

declare arch :: Architecture

bool is32Bit(a::Architecture) = arch == RV32Sv32

---------------------------------------------------------------------------
-- Privilege levels
---------------------------------------------------------------------------

construct Privilege
{ User
, Supervisor
, Hypervisor
, Trusted
}

priv_level privLevel(p::Privilege) =
    match p
    {
      case User       => 0
      case Supervisor => 1
      case Hypervisor => 2
      case Trusted    => 3
    }

---------------------------------------------------------------------------
-- Exceptions and Interrupts
---------------------------------------------------------------------------

construct Interrupt
{ IPI
, Host
, Timer
}

nat interruptIndex(i::Interrupt) =
    match i
    {
      case IPI   => 5
      case Host  => 6
      case Timer => 7
    }

construct ExceptionType
{ Fetch_Misaligned
, Fetch_Fault
, Illegal_Instr
, Priv_Instr
, FP_Disabled
, Syscall
, Breakpoint
, Load_Misaligned
, Store_Misaligned
, Load_Fault
, Store_Fault
}

exc_code excCode(e::ExceptionType) =
    match e
    {
      case Fetch_Misaligned   => 0x0
      case Fetch_Fault        => 0x1
      case Illegal_Instr      => 0x2
      case Priv_Instr         => 0x3
      case FP_Disabled        => 0x4

      case Syscall            => 0x6
      case Breakpoint         => 0x7
      case Load_Misaligned    => 0x8
      case Store_Misaligned   => 0x9
      case Load_Fault         => 0xa
      case Store_Fault        => 0xb
    }

ExceptionType excType(e::exc_code) =
    match e
    {
      case 0x0 => Fetch_Misaligned
      case 0x1 => Fetch_Fault
      case 0x2 => Illegal_Instr
      case 0x3 => Priv_Instr
      case 0x4 => FP_Disabled

      case 0x6 => Syscall
      case 0x7 => Breakpoint
      case 0x8 => Load_Misaligned
      case 0x9 => Store_Misaligned
      case 0xa => Load_Fault
      case 0xb => Store_Fault
      case _   => #UNDEFINED("Unknown exception")
    }

string exceptionName(e::ExceptionType) =
    match e
    {
      case Fetch_Misaligned   => "MISALIGNED_FETCH"
      case Fetch_Fault        => "FAULT_FETCH"
      case Illegal_Instr      => "ILLEGAL_INSTRUCTION"
      case Priv_Instr         => "PRIVILEGED_INSTRUCTION"
      case FP_Disabled        => "FP_DISABLED"

      case Syscall            => "SYSCALL"
      case Breakpoint         => "BREAKPOINT"
      case Load_Misaligned    => "MISALIGNED_LOAD"
      case Store_Misaligned   => "MISALIGNED_STORE"
      case Load_Fault         => "FAULT_LOAD"
      case Store_Fault        => "FAULT_STORE"
    }

regType makeExceptionCause(e::ExceptionType) =
    [excCode(e)]

bool isBadAddressException(e::ExceptionType) =
    match e
    {
      case Load_Misaligned
      or   Store_Misaligned
      or   Load_Fault
      or   Store_Fault => true
      case _           => false
    }

---------------------------------------------------------------------------
-- Control and Status Registers
---------------------------------------------------------------------------

register status :: regType
{
  31-24 : IP    -- Pending Interrupts
  23-16 : IM    -- Interrupt Mask
      7 : VM    -- Virtual memory enabled (XXX: bit location not in 1.99 spec)
      6 : S64   -- RV64Sv43 in supervisor mode support
      5 : U64   -- RV64 in user mode support
      4 : EF    -- Enabled floating-point
      3 : PEI   -- Previous EI
      2 : EI    -- Enabled Interrupts
      1 : PS    -- Previous S
      0 : S     -- Supervisor mode
}

register cause :: regType
{
     63 : Int   -- Interrupt
    4-0 : EC    -- Exception Code
}

bool is_reserved_CSR(rp::csrAR, wp::csrAR) =
    match rp, wp
    {
      case 1, 0 => true
      case 2, 0 => true
      case 2, 1 => true
      case _, _ => false
    }

-- This function assumes that the CSR is not reserved.
bool check_CSR_access(rp::csrAR, wp::csrAR, p::Privilege, a::accessType) =
{
  pl = privLevel(p);
  if rp <> 0b11 then
      pl >= (if a == Read then rp else wp)
  else
      if a == Write then p == Trusted
      else               pl >= rp
}

-- XXX: The supervisor spec, v1.99, does not explicitly define *which*
-- exception is raised when a CSR is accessed without appropriate
-- privilege, or when a non-existent CSR is accessed.

-- Update: This is fixed in the *later* v1.7 spec, where it says that
-- the illegal instruction exception is raised in both cases.

---------------------------------------------------------------------------
-- Register state space
---------------------------------------------------------------------------

-- Each register state is local to a core.

type RegFile    = reg  -> regType

record UserCSR
{
  cycle         :: id -> regType
  time          :: id -> regType
  instret       :: id -> regType
  cycleh        :: id -> regType
  timeh         :: id -> regType
  instreth      :: id -> regType
}

record SystemCSR
{
  sup0          :: regType    -- 0x500: SR/SW: scratch register for exception handlers
  sup1          :: regType    -- 0x501: SR/SW: scratch register for exception handlers
  epc           :: regType    -- 0x502: SR/SW: exception program counter

  badvaddr      :: regType    -- 0x503: SR:    bad virtual address

  ptbr          :: regType    -- 0x504: SR/SW: page table base register
  asid          :: regType    -- 0x505: SR/SW: address space ID
  count         :: regType    -- 0x506: SR/SW: cycle counter for timer
  compare       :: regType    -- 0x507: SR/SW: timer compare value
  evec          :: regType    -- 0x508: SR/SW: exception handler address

  cause         :: cause      -- 0x509: SR:    cause of exception

  status        :: status     -- 0x50a: SR/SW: status register

  hartid        :: regType    -- 0x50b: SR:    hardware thread ID
  impl          :: regType    -- 0x50c: SR:    implementation ID

  fatc          :: regType    -- 0x50d: SW:    flush address translation cache
  send_ipi      :: regType    -- 0x50e: SW:    send inter-processor interrupt
  clear_ipi     :: regType    -- 0x50f: SW:    clear inter-processor interrupt

  tohost        :: regType    -- 0x51e: SR/SW: test output register
  fromhost      :: regType    -- 0x51f: SR/SW: test input register
}

declare
{
  c_gpr         :: id -> RegFile                -- general purpose registers
  c_PC          :: id -> regType                -- program counter
  c_BranchTo    :: id -> regType option         -- requested branch

  c_UCSR        :: id -> UserCSR                -- user-mode accessible CSRs
  c_SCSR        :: id -> SystemCSR              -- system-level CSRs

  c_Exception   :: id -> ExceptionType option   -- exception
}
-- Based on Table 1.4 of spec version 1.99.
bool is_CSR_defined(csr::creg) =
    (csr >= 0x001 and csr <= 0x003)
 or (csr >= 0x500 and csr <= 0x50F)
 or (csr >= 0x51E and csr <= 0x51F)
 or (csr >= 0xC00 and csr <= 0xC02)
 or ((csr >= 0xC80 and csr <= 0xC82) and is32Bit(arch))

reg STACK = 2

-- Instruction counter
declare instCnt :: nat

-- Number of cores
declare totalCore :: nat

-- ID of the core executing current instruction
declare procID :: id

-- The following components provide read/write access to state of the
-- core whose id equals procID.  For example, writing "gpr(r)" refers
-- general purpose register "r" in the core whose id equals procID.

component gpr(n::reg) :: regType
{
  value         = { m = c_gpr(procID); m(n) }
  assign value  = { var m = c_gpr(procID)
                  ; m(n) <- value
                  ; c_gpr(procID) <- m
                  }
}

component PC :: regType
{
  value         = c_PC(procID)
  assign value  = c_PC(procID) <- value
}

component BranchTo :: regType option
{
   value        = c_BranchTo(procID)
   assign value = c_BranchTo(procID) <- value
}

component Exception :: ExceptionType option
{
   value        = c_Exception(procID)
   assign value = c_Exception(procID) <- value
}

component SCSR :: SystemCSR
{
   value        = c_SCSR(procID)
   assign value = c_SCSR(procID) <- value
}

component UCSR :: UserCSR
{
   value        = c_UCSR(procID)
   assign value = c_UCSR(procID) <- value
}

bool notWordValue(value::regType) =
{
  top = value<63:32>;
  if value<31> then
      top <> 0xFFFF_FFFF
  else
      top <> 0x0
}

---------------------------------------------------------------------------
-- Tandem verification
---------------------------------------------------------------------------
-- This describes the state update due to every retired instruction,
-- which can be verified against an external oracle.  Currently, the
-- Cissr tool from Bluespec fills the role, and the record below is
-- designed against its API.

record StateDelta
{
  exc_taken     :: bool         -- whether an exception (interrupt/trap) was taken
  pc            :: regType      -- PC of retired instruction

  addr          :: regType      -- address argument for instruction:
                                --   new control flow target for jump, exception branch, SRET
                                --   memory address for memory ops and AMOs
                                --   CSR register address for CSR instructions

  data1         :: regType      -- data result for instruction:
                                --   new value for rd for ALU ops, LOAD, LOAD_FP, LR, SC, CSR ops
                                --   new csr_status for exceptions and SRET

  data2         :: regType      -- data argument for instruction:
                                --   new csr_cause for exceptions
                                --   new memory value for STORE, STORE_FP, SC, AMOs
                                --   argument for CSR ops

  data3         :: regType      -- instruction that just executed

  fp_data       :: fpval        -- floating point value
}

declare c_update :: id -> StateDelta

component Delta :: StateDelta
{
   value         = c_update(procID)
   assign value  = c_update(procID) <- value
}

unit setupDelta(pc::regType, instr_word::word) =
{
  Delta.pc      <- pc;
  Delta.data3   <- ZeroExtend(instr_word)
}

---------------------------------------------------------------------------
-- Logging
---------------------------------------------------------------------------
string reg(r::reg) =
{
  if      r ==  0 then "$0"
  else if r ==  1 then "ra"
  else if r ==  2 then "s0"
  else if r ==  3 then "s1"
  else if r ==  4 then "s2"
  else if r ==  5 then "s3"
  else if r ==  6 then "s4"
  else if r ==  7 then "s5"
  else if r ==  8 then "s6"
  else if r ==  9 then "s7"
  else if r == 10 then "s8"
  else if r == 11 then "s9"
  else if r == 12 then "s10"
  else if r == 13 then "s11"
  else if r == 14 then "sp"
  else if r == 15 then "tp"
  else if r == 16 then "v0"
  else if r == 17 then "v1"
  else if r == 18 then "a0"
  else if r == 19 then "a1"
  else if r == 20 then "a2"
  else if r == 21 then "a3"
  else if r == 22 then "a4"
  else if r == 23 then "a5"
  else if r == 24 then "a6"
  else if r == 25 then "a7"
  else if r == 26 then "t0"
  else if r == 27 then "t1"
  else if r == 28 then "t2"
  else if r == 29 then "t3"
  else if r == 30 then "t4"
  else                 "gp"
}

string log_w_gpr(r::reg, data::regType) =
    "Reg " : reg(r) : " (" : [[r]::nat] : ") <- 0x" : PadLeft(#"0", 16, [data])

string log_w_mem_mask(pAddr::pAddr, vAddr::vAddr, mask::regType, data::regType) =
    "MEM[0x" : PadLeft(#"0", 10, [pAddr]) :
    "/" : PadLeft(#"0", 10, [vAddr]) :
    "] <- (data: 0x" : PadLeft(#"0", 16, [data]) :
    ", mask: 0x" : PadLeft(#"0", 16, [mask]) : ")"

string log_w_mem(pAddr::pAddr, vAddr::vAddr, data::regType) =
    "MEM[0x" : PadLeft(#"0", 10, [pAddr]) :
    "/" : PadLeft(#"0", 10, [vAddr]) :
    "] <- (data: 0x" : PadLeft(#"0", 16, [data]) : ")"

string log_r_mem(pAddr::pAddr, vAddr::vAddr, data::regType) =
    "data <- MEM[0x" : PadLeft(#"0", 10, [pAddr]) :
    "/" : PadLeft(#"0", 10, [vAddr]) :
    "]: 0x" : PadLeft(#"0", 16, [data])

declare log :: nat -> string list   -- One log per "trace level"

unit mark_log(lvl::nat, s::string) = log(lvl) <- s @ log(lvl)
unit unmark_log(lvl::nat) = log(lvl) <- Tail(log(lvl))
unit clear_logs() = for i in 0 .. 5 do log(i) <- Nil

string hex32(x::word)  = PadLeft(#"0", 8, [x])
string hex64(x::dword) = PadLeft(#"0", 16, [x])

---------------------------------------------------------------------------
-- Exceptions
---------------------------------------------------------------------------

unit setupException(e::ExceptionType) =
{
  SCSR.cause.Int    <- false;
  SCSR.cause.EC     <- excCode(e);
  SCSR.epc          <- PC;
  SCSR.status.PS    <- SCSR.status.S;
  SCSR.status.S     <- true;
  SCSR.status.PEI   <- SCSR.status.EI;
  SCSR.status.EI    <- false;
  Exception         <- Some(e)
}

unit signalAddressException(e::ExceptionType, vAddr::vAddr) =
{
  SCSR.badvaddr     <- vAddr;
  setupException(e)
}

unit signalException(e::ExceptionType) =
{
  SCSR.badvaddr     <- 0;
  setupException(e)
}

---------------------------------------------------------------------------
-- GPR access with logging
---------------------------------------------------------------------------

component GPR(n::reg) :: regType
{
  value = if n == 0 then 0 else gpr(n)
  assign value = when n <> 0 do
                   { gpr(n) <- value
                   ; mark_log(2, log_w_gpr(n, value))
                   }
}

unit writeRD(rd::reg, val::regType) =
{
  GPR(rd)       <- val;
  Delta.data1   <- val
}

unit dumpRegs() =
{
    mark_log(0, "======   Registers   ======")
  ; mark_log(0, "Core = " : [[procID]::nat])
  ; mark_log(0, "PC   0x" : hex64(PC))
  ; for i in 0 .. 31 do
      mark_log(0, "Reg " : (if i < 10 then " " else "") : [i] : " 0x" :
               hex64(GPR([i])))
}

---------------------------------------------------------------------------
-- Memory access
---------------------------------------------------------------------------

declare VMEM :: pAddr -> regType -- user-space virtual memory,
                                 -- aligned at 2^(|vAddr| - |pAddr|)

unit initVMEM = VMEM <- InitMap(0x0)

regType readData(vAddr::vAddr) =
{
  pAddr = vAddr<63:3>;
  data  = VMEM(pAddr);
  mark_log(2, log_r_mem(pAddr, vAddr, data));
  data
}

unit writeData(vAddr::vAddr, rs2::reg, mask::regType) =
{
  data  = GPR(rs2);
  pAddr = vAddr<63:3>;
  val   = VMEM(pAddr) && ~mask || data && mask;
  VMEM(pAddr) <- val;
  Delta.addr  <- vAddr;
  Delta.data2 <- val;
  mark_log(2, log_w_mem_mask(pAddr, vAddr, mask, data))
}

word readInst(vAddr::vAddr) =
{
  pAddr = vAddr<63:3>;
  data = VMEM(pAddr);
  mark_log(2, log_r_mem(pAddr, vAddr, data));
  if vAddr<2> then data<63:32> else data<31:0>
}

-- helper used to preload memory contents
unit writeMem(vAddr::vAddr, data::regType) =
{
  pAddr = vAddr<63:3>;
  VMEM(pAddr) <- data;
  mark_log(2, log_w_mem(pAddr, vAddr, data))
}

--------------------------------------------------
-- Instruction fetch
--------------------------------------------------

word option Fetch() =
{
  pc    = PC;
  inst  = readInst(pc);
  setupDelta(pc, inst);
  Some(inst)
}

---------------------------------------------------------------------------
-- Integer Computational Instructions
---------------------------------------------------------------------------

-- Integer register-immediate

-----------------------------------
-- ADDI  rd, rs1, imm
-----------------------------------
define ArithI > ADDI(rd::reg, rs1::reg, imm::imm12) =
    writeRD(rd, GPR(rs1) + SignExtend(imm))

-----------------------------------
-- ADDIW rd, rs1, imm   (RV64I)
-----------------------------------
define ArithI > ADDIW(rd::reg, rs1::reg, imm::imm12) =
    if is32Bit(arch) then
        signalException(Illegal_Instr)
    else {
      temp = GPR(rs1) + SignExtend(imm);
      writeRD(rd, SignExtend(temp<31:0>))
    }

-----------------------------------
-- SLTI  rd, rs1, imm
-----------------------------------
define ArithI > SLTI(rd::reg, rs1::reg, imm::imm12) =
    writeRD(rd, [GPR(rs1) < SignExtend(imm)])

-----------------------------------
-- SLTIU rd, rs1, imm
-----------------------------------
define ArithI > SLTIU(rd::reg, rs1::reg, imm::imm12) =
    writeRD(rd, [GPR(rs1) <+ SignExtend(imm)])

-- NOTE: RISCV ANDI/ORI/XORI use sign-extended 12-bit immediates,
-- unlike zero-extended 16-bit immediates in MIPS.

-----------------------------------
-- ANDI  rd, rs1, imm
-----------------------------------
define ArithI > ANDI(rd::reg, rs1::reg, imm::imm12) =
    writeRD(rd, GPR(rs1) && SignExtend(imm))

-----------------------------------
-- ORI   rd, rs1, imm
-----------------------------------
define ArithI > ORI(rd::reg, rs1::reg, imm::imm12) =
    writeRD(rd, GPR(rs1) || SignExtend(imm))

-----------------------------------
-- XORI  rd, rs1, imm
-----------------------------------
define ArithI > XORI(rd::reg, rs1::reg, imm::imm12) =
    writeRD(rd, GPR(rs1) ?? SignExtend(imm))


-- NOTE: RISCV SSLI/SRLI/SRAI use zero-extended 32-bit results, unlike
-- sign-extended results in MIPS.

-----------------------------------
-- SLLI  rd, rs1, imm
-----------------------------------
define Shift > SLLI(rd::reg, rs1::reg, imm::bits(6)) =
    if is32Bit(arch) and imm<5> then
        signalException(Illegal_Instr)
    else
        writeRD(rd, GPR(rs1) << [imm])

-----------------------------------
-- SRLI  rd, rs1, imm
-----------------------------------
define Shift > SRLI(rd::reg, rs1::reg, imm::bits(6)) =
    if is32Bit(arch) and imm<5> then
        signalException(Illegal_Instr)
    else
        writeRD(rd, GPR(rs1) >>+ [imm])

-----------------------------------
-- SRAI  rd, rs1, imm
-----------------------------------
define Shift > SRAI(rd::reg, rs1::reg, imm::bits(6)) =
    if is32Bit(arch) and imm<5> then
        signalException(Illegal_Instr)
    else
        writeRD(rd, GPR(rs1) >> [imm])

-----------------------------------
-- SLLIW rd, rs1, imm   (RV64I)
-----------------------------------
define Shift > SLLIW(rd::reg, rs1::reg, imm::bits(5)) =
    if is32Bit(arch) or notWordValue(GPR(rs1)) then
        signalException(Illegal_Instr)
    else
        writeRD(rd, SignExtend(GPR(rs1)<31:0> << [imm]))

-----------------------------------
-- SRLIW rd, rs1, imm   (RV64I)
-----------------------------------
define Shift > SRLIW(rd::reg, rs1::reg, imm::bits(5)) =
    if is32Bit(arch) or notWordValue(GPR(rs1)) then
        signalException(Illegal_Instr)
    else
        writeRD(rd, SignExtend(GPR(rs1)<31:0> >>+ [imm]))

-----------------------------------
-- SRAIW rd, rs1, imm   (RV64I)
-----------------------------------
define Shift > SRAIW(rd::reg, rs1::reg, imm::bits(5)) =
    if is32Bit(arch) or notWordValue(GPR(rs1)) then
        signalException(Illegal_Instr)
    else
        writeRD(rd, SignExtend(GPR(rs1)<31:0> >> [imm]))

-----------------------------------
-- LUI   rd, imm
-----------------------------------
define ArithI > LUI(rd::reg, imm::imm20) =
    writeRD(rd, SignExtend(imm : 0`12))

-----------------------------------
-- AUIPC rd, imm
-----------------------------------
-- XXX: the description for RV64I is ambiguous: it doesn't specify the
-- intermediate 32-bit value with zero-filled lowest 12 bits before
-- sign-extension.

define ArithI > AUIPC(rd::reg, imm::imm20) =
    writeRD(rd, PC + SignExtend(imm : 0`12))


-- Integer register-register

-----------------------------------
-- ADD   rd, rs1, rs2
-----------------------------------
define ArithR > ADD(rd::reg, rs1::reg, rs2::reg) =
    writeRD(rd, GPR(rs1) + GPR(rs2))

-----------------------------------
-- ADDW  rd, rs1, rs2   (RV64I)
-----------------------------------
define ArithR > ADDW(rd::reg, rs1::reg, rs2::reg) =
    if is32Bit(arch) then
        signalException(Illegal_Instr)
    else
        writeRD(rd, SignExtend(GPR(rs1)<31:0> + GPR(rs2)<31:0>))

-----------------------------------
-- SUB   rd, rs1, rs2
-----------------------------------
define ArithR > SUB(rd::reg, rs1::reg, rs2::reg) =
    writeRD(rd, GPR(rs1) - GPR(rs2))

-----------------------------------
-- SUBW  rd, rs1, rs2   (RV64I)
-----------------------------------
define ArithR > SUBW(rd::reg, rs1::reg, rs2::reg) =
    if is32Bit(arch) then
        signalException(Illegal_Instr)
    else
        writeRD(rd, SignExtend(GPR(rs1)<31:0> - GPR(rs2)<31:0>))

-----------------------------------
-- SLT   rd, rs1, rs2
-----------------------------------
define ArithR > SLT(rd::reg, rs1::reg, rs2::reg) =
    writeRD(rd, [GPR(rs1) < GPR(rs2)])

-----------------------------------
-- SLTU  rd, rs1, rs2
-----------------------------------
define ArithR > SLTU(rd::reg, rs1::reg, rs2::reg) =
    writeRD(rd, [GPR(rs1) <+ GPR(rs2)])

-----------------------------------
-- AND   rd, rs1, rs2
-----------------------------------
define ArithR > AND(rd::reg, rs1::reg, rs2::reg) =
    writeRD(rd, GPR(rs1) && GPR(rs2))

-----------------------------------
-- OR    rd, rs1, rs2
-----------------------------------
define ArithR > OR(rd::reg, rs1::reg, rs2::reg) =
    writeRD(rd, GPR(rs1) || GPR(rs2))

-----------------------------------
-- XOR   rd, rs1, rs2
-----------------------------------
define ArithR > XOR(rd::reg, rs1::reg, rs2::reg) =
    writeRD(rd, GPR(rs1) ?? GPR(rs2))

-----------------------------------
-- SLL   rd, rs1, rs2
-----------------------------------
define Shift > SLL(rd::reg, rs1::reg, rs2::reg) =
    if is32Bit(arch) then
        writeRD(rd, GPR(rs1) << ZeroExtend(GPR(rs2)<4:0>))
    else
        writeRD(rd, GPR(rs1) << ZeroExtend(GPR(rs2)<5:0>))

-----------------------------------
-- SLLW  rd, rs1, rs2   (RV64I)
-----------------------------------
define Shift > SLLW(rd::reg, rs1::reg, rs2::reg) =
    if is32Bit(arch) then
        signalException(Illegal_Instr)
    else
        writeRD(rd, SignExtend(GPR(rs1)<31:0> << ZeroExtend(GPR(rs2)<4:0>)))

-----------------------------------
-- SRL   rd, rs1, rs2
-----------------------------------
define Shift > SRL(rd::reg, rs1::reg, rs2::reg) =
    if is32Bit(arch) then
        writeRD(rd, GPR(rs1) >>+ ZeroExtend(GPR(rs2)<4:0>))
    else
        writeRD(rd, GPR(rs1) >>+ ZeroExtend(GPR(rs2)<5:0>))

-----------------------------------
-- SRLW  rd, rs1, rs2   (RV64I)
-----------------------------------
define Shift > SRLW(rd::reg, rs1::reg, rs2::reg) =
    if is32Bit(arch) then
        signalException(Illegal_Instr)
    else
        writeRD(rd, SignExtend(GPR(rs1)<31:0> >>+ ZeroExtend(GPR(rs2)<4:0>)))

-----------------------------------
-- SRA   rd, rs1, rs2
-----------------------------------
define Shift > SRA(rd::reg, rs1::reg, rs2::reg) =
    if is32Bit(arch) then
        writeRD(rd, GPR(rs1) >> ZeroExtend(GPR(rs2)<4:0>))
    else
        writeRD(rd, GPR(rs1) >> ZeroExtend(GPR(rs2)<5:0>))

-----------------------------------
-- SRAW  rd, rs1, rs2   (RV64I)
-----------------------------------
define Shift > SRAW(rd::reg, rs1::reg, rs2::reg) =
    if is32Bit(arch) then
        signalException(Illegal_Instr)
    else
        writeRD(rd, SignExtend(GPR(rs1)<31:0> >> ZeroExtend(GPR(rs2)<4:0>)))

---------------------------------------------------------------------------
-- Control Transfer Instructions
---------------------------------------------------------------------------

-- Unconditional jumps

-----------------------------------
-- JAL   rd, offs
-----------------------------------
define Branch > JAL(rd::reg, imm::imm20) =
{
   GPR(rd)  <- PC + 4;
   BranchTo <- Some(PC + SignExtend(imm << 1))
}

-----------------------------------
-- JALR  rd, rs1, imm
-----------------------------------
define Branch > JALR(rd::reg, rs1::reg, imm::imm12) =
{
  temp      = GPR(rs1) + SignExtend(imm);
  GPR(rd)  <- PC + 4;
  BranchTo <- Some(temp && SignExtend('10'))
}

-- conditional branches

-----------------------------------
-- BEQ   rs1, rs2, offs
-----------------------------------
define Branch > BEQ(rs1::reg, rs2::reg, offs::imm12) =
    if GPR(rs1) == GPR(rs2) then
        BranchTo <- Some(PC + (SignExtend(offs) << 1))
    else
        ()

-----------------------------------
-- BNE   rs1, rs2, offs
-----------------------------------
define Branch > BNE(rs1::reg, rs2::reg, offs::imm12) =
    if GPR(rs1) <> GPR(rs2) then
        BranchTo <- Some(PC + (SignExtend(offs) << 1))
    else
        ()

-----------------------------------
-- BLT   rs1, rs2, offs
-----------------------------------
define Branch > BLT(rs1::reg, rs2::reg, offs::imm12) =
    if GPR(rs1) < GPR(rs2) then
        BranchTo <- Some(PC + (SignExtend(offs) << 1))
    else
        ()

-----------------------------------
-- BLTU  rs1, rs2, offs
-----------------------------------
define Branch > BLTU(rs1::reg, rs2::reg, offs::imm12) =
    if GPR(rs1) <+ GPR(rs2) then
        BranchTo <- Some(PC + (SignExtend(offs) << 1))
    else
        ()

-----------------------------------
-- BGE   rs1, rs2, offs
-----------------------------------
define Branch > BGE(rs1::reg, rs2::reg, offs::imm12) =
    if GPR(rs1) >= GPR(rs2) then
        BranchTo <- Some(PC + (SignExtend(offs) << 1))
    else
        ()

-----------------------------------
-- BGEU  rs1, rs2, offs
-----------------------------------
define Branch > BGEU(rs1::reg, rs2::reg, offs::imm12) =
    if GPR(rs1) >=+ GPR(rs2) then
        BranchTo <- Some(PC + (SignExtend(offs) << 1))
    else
        ()

---------------------------------------------------------------------------
-- Load and Store Instructions
---------------------------------------------------------------------------

-----------------------------------
-- LW    rd, rs1, offs
-----------------------------------
define Load > LW(rd::reg, rs1::reg, offs::imm12) =
{
  addr = GPR(rs1) + SignExtend(offs);
  val  = readData(addr);
  GPR(rd) <- SignExtend(val<31:0>)
}

-----------------------------------
-- LWU   rd, rs1, offs  (RV64I)
-----------------------------------
define Load > LWU(rd::reg, rs1::reg, offs::imm12) =
{
  if is32Bit(arch) then
      signalException(Illegal_Instr)
  else {
    addr = GPR(rs1) + SignExtend(offs);
    val  = readData(addr);
    GPR(rd) <- ZeroExtend(val<31:0>)
  }
}

-----------------------------------
-- LH    rd, rs1, offs
-----------------------------------
define Load > LH(rd::reg, rs1::reg, offs::imm12) =
{
  addr = GPR(rs1) + SignExtend(offs);
  val  = readData(addr);
  GPR(rd) <- SignExtend(val<15:0>)
}

-----------------------------------
-- LHU   rd, rs1, offs
-----------------------------------
define Load > LHU(rd::reg, rs1::reg, offs::imm12) =
{
  addr = GPR(rs1) + SignExtend(offs);
  val  = readData(addr);
  GPR(rd) <- ZeroExtend(val<15:0>)
}

-----------------------------------
-- LB    rd, rs1, offs
-----------------------------------
define Load > LB(rd::reg, rs1::reg, offs::imm12) =
{
  addr = GPR(rs1) + SignExtend(offs);
  val  = readData(addr);
  GPR(rd) <- SignExtend(val<7:0>)
}

-----------------------------------
-- LBU   rd, rs1, offs
-----------------------------------
define Load > LBU(rd::reg, rs1::reg, offs::imm12) =
{
  addr = GPR(rs1) + SignExtend(offs);
  val  = readData(addr);
  GPR(rd) <- ZeroExtend(val<7:0>)
}

-----------------------------------
-- LD    rd, rs1, offs  (RV64I)
-----------------------------------
define Load > LD(rd::reg, rs1::reg, offs::imm12) =
    if is32Bit(arch) then
        signalException(Illegal_Instr)
    else {
      addr = GPR(rs1) + SignExtend(offs);
      val  = readData(addr);
      GPR(rd) <- val
    }

-----------------------------------
-- SW    rs1, rs2, offs
-----------------------------------
define Store > SW(rs1::reg, rs2::reg, offs::imm12) =
{
  addr = GPR(rs1) + SignExtend(offs);
  mask = 0xFFFF_FFFF;
  writeData(addr, rs2, mask)
}

-----------------------------------
-- SH    rs1, rs2, offs
-----------------------------------
define Store > SH(rs1::reg, rs2::reg, offs::imm12) =
{
  addr = GPR(rs1) + SignExtend(offs);
  mask = 0xFFFF;
  writeData(addr, rs2, mask)
}

-----------------------------------
-- SB    rs1, rs2, offs
-----------------------------------
define Store > SB(rs1::reg, rs2::reg, offs::imm12) =
{
  addr = GPR(rs1) + SignExtend(offs);
  mask = 0xFF;
  writeData(addr, rs2, mask)
}

-----------------------------------
-- SD    rs1, rs2, offs (RV64I)
-----------------------------------
define Store > SD(rs1::reg, rs2::reg, offs::imm12) =
    if is32Bit(arch) then
        signalException(Illegal_Instr)
    else {
      addr = GPR(rs1) + SignExtend(offs);
      writeData(addr, rs2, SignExtend('1'))
    }

---------------------------------------------------------------------------
-- Memory model
---------------------------------------------------------------------------

-----------------------------------
-- FENCE rd, rs1, pred, succ
-----------------------------------
define FENCE(rd::reg, rs1::reg, pred::bits(4), succ::bits(4)) = nothing

-----------------------------------
-- FENCE.I rd, rs1, imm
-----------------------------------
define FENCE_I(rd::reg, rs1::reg, imm::imm12) = nothing

---------------------------------------------------------------------------
-- System Instructions
---------------------------------------------------------------------------

-----------------------------------
-- SCALL
-----------------------------------
define System > SCALL  = signalException(Syscall)

-----------------------------------
-- SBREAK
-----------------------------------
-- XXX: The spec doesn't explicitly say that the Breakpoint exception
-- is raised by SBREAK.  Fixed in the *later* 1.7 spec.

define System > SBREAK = signalException(Breakpoint)

-----------------------------------
-- SCALL
-----------------------------------
define System >  SRET  = nothing

-- Timers and Counters

-----------------------------------
-- CSRRW  rd, rs1, imm
-----------------------------------
define System > CSRRW(rd::reg, rs1::reg, csr::imm12) = nothing

-----------------------------------
-- CSRRS  rd, rs1, imm
-----------------------------------
define System > CSRRS(rd::reg, rs1::reg, csr::imm12) = nothing

-----------------------------------
-- CSRRC  rd, rs1, imm
-----------------------------------
define System > CSRRC(rd::reg, rs1::reg, csr::imm12) = nothing

-----------------------------------
-- CSRRWI rd, rs1, imm
-----------------------------------
define System > CSRRWI(rd::reg, rs1::reg, csr::imm12) = nothing

-----------------------------------
-- CSRRSI rd, rs1, imm
-----------------------------------
define System > CSRRSI(rd::reg, rs1::reg, csr::imm12) = nothing

-----------------------------------
-- CSRRCI rd, rs1, imm
-----------------------------------
define System > CSRRCI(rd::reg, rs1::reg, csr::imm12) = nothing

-----------------------------------
-- Unsupported instructions
-----------------------------------
define UnknownInstruction =
   signalException(Illegal_Instr)

define Run

---------------------------------------------------------------------------
-- Instruction decoding
---------------------------------------------------------------------------

-- helpers to assemble various immediates from their pieces
imm12 asImm12(imm12::bits(1), imm11::bits(1), immhi::bits(6), immlo::bits(4)) =
    imm12 : imm11 : immhi : immlo

imm20 asImm20(imm20::bits(1), immhi::bits(8), imm11::bits(1), immlo::bits(10)) =
    imm20 : immhi : imm11 : immlo

imm12 asSImm12(immhi::bits(7), immlo::bits(5)) =  immhi : immlo

instruction Decode(w::word) =
   match w
   {
     case 'i12 ihi rs2 rs1 000 ilo i11 11000 11' => Branch( BEQ(rs1, rs2, asImm12(i12, i11, ihi, ilo)))
     case 'i12 ihi rs2 rs1 001 ilo i11 11000 11' => Branch( BNE(rs1, rs2, asImm12(i12, i11, ihi, ilo)))
     case 'i12 ihi rs2 rs1 100 ilo i11 11000 11' => Branch( BLT(rs1, rs2, asImm12(i12, i11, ihi, ilo)))
     case 'i12 ihi rs2 rs1 101 ilo i11 11000 11' => Branch( BGE(rs1, rs2, asImm12(i12, i11, ihi, ilo)))
     case 'i12 ihi rs2 rs1 110 ilo i11 11000 11' => Branch(BLTU(rs1, rs2, asImm12(i12, i11, ihi, ilo)))
     case 'i12 ihi rs2 rs1 111 ilo i11 11000 11' => Branch(BGEU(rs1, rs2, asImm12(i12, i11, ihi, ilo)))

     case 'imm           rs1 000  rd 11001 11' => Branch( JALR(rd, rs1, imm))
     case 'i20 ilo i11 ihi        rd 11011 11' => Branch(  JAL(rd, asImm20(i20, ihi, i11, ilo)))

     case 'imm                    rd 01101 11' => ArithI(  LUI(rd, imm))
     case 'imm                    rd 00101 11' => ArithI(AUIPC(rd, imm))

     case 'imm           rs1 000  rd 00100 11' => ArithI( ADDI(rd, rs1, imm))
     case '000000  shamt rs1 001  rd 00100 11' =>  Shift( SLLI(rd, rs1, shamt))
     case 'imm           rs1 010  rd 00100 11' => ArithI( SLTI(rd, rs1, imm))
     case 'imm           rs1 011  rd 00100 11' => ArithI(SLTIU(rd, rs1, imm))
     case 'imm           rs1 100  rd 00100 11' => ArithI( XORI(rd, rs1, imm))
     case '000000  shamt rs1 101  rd 00100 11' =>  Shift( SRLI(rd, rs1, shamt))
     case '010000  shamt rs1 101  rd 00100 11' =>  Shift( SRAI(rd, rs1, shamt))
     case 'imm           rs1 110  rd 00100 11' => ArithI(  ORI(rd, rs1, imm))
     case 'imm           rs1 111  rd 00100 11' => ArithI( ANDI(rd, rs1, imm))

     case '0000000   rs2 rs1 000  rd 01100 11' => ArithR(  ADD(rd, rs1, rs2))
     case '0100000   rs2 rs1 000  rd 01100 11' => ArithR(  SUB(rd, rs1, rs2))
     case '0000000   rs2 rs1 001  rd 01100 11' =>  Shift(  SLL(rd, rs1, rs2))
     case '0000000   rs2 rs1 010  rd 01100 11' => ArithR(  SLT(rd, rs1, rs2))
     case '0000000   rs2 rs1 011  rd 01100 11' => ArithR( SLTU(rd, rs1, rs2))
     case '0000000   rs2 rs1 100  rd 01100 11' => ArithR(  XOR(rd, rs1, rs2))
     case '0000000   rs2 rs1 101  rd 01100 11' =>  Shift(  SRL(rd, rs1, rs2))
     case '0100000   rs2 rs1 101  rd 01100 11' =>  Shift(  SRA(rd, rs1, rs2))
     case '0000000   rs2 rs1 110  rd 01100 11' => ArithR(   OR(rd, rs1, rs2))
     case '0000000   rs2 rs1 111  rd 01100 11' => ArithR(  AND(rd, rs1, rs2))

     case 'imm           rs1 000  rd 00110 11' => ArithI(ADDIW(rd, rs1, imm))
     case '0000000 shamt rs1 001  rd 00110 11' =>  Shift(SLLIW(rd, rs1, shamt))
     case '0000000 shamt rs1 101  rd 00110 11' =>  Shift(SRLIW(rd, rs1, shamt))
     case '0100000 shamt rs1 101  rd 00110 11' =>  Shift(SRAIW(rd, rs1, shamt))

     case '0000000   rs2 rs1 000  rd 01110 11' => ArithR( ADDW(rd, rs1, rs2))
     case '0100000   rs2 rs1 000  rd 01110 11' => ArithR( SUBW(rd, rs1, rs2))
     case '0000000   rs2 rs1 001  rd 01110 11' =>  Shift( SLLW(rd, rs1, rs2))
     case '0000000   rs2 rs1 101  rd 01110 11' =>  Shift( SRLW(rd, rs1, rs2))
     case '0100000   rs2 rs1 101  rd 01110 11' =>  Shift( SRAW(rd, rs1, rs2))

     case 'imm           rs1 000  rd 00000 11' =>   Load(   LB(rd, rs1, imm))
     case 'imm           rs1 001  rd 00000 11' =>   Load(   LH(rd, rs1, imm))
     case 'imm           rs1 010  rd 00000 11' =>   Load(   LW(rd, rs1, imm))
     case 'imm           rs1 011  rd 00000 11' =>   Load(   LD(rd, rs1, imm))
     case 'imm           rs1 100  rd 00000 11' =>   Load(  LBU(rd, rs1, imm))
     case 'imm           rs1 101  rd 00000 11' =>   Load(  LHU(rd, rs1, imm))
     case 'imm           rs1 110  rd 00000 11' =>   Load(  LWU(rd, rs1, imm))

     case 'ihi       rs2 rs1 000 ilo 01000 11' =>  Store(   SB(rs1, rs2, asSImm12(ihi, ilo)))
     case 'ihi       rs2 rs1 001 ilo 01000 11' =>  Store(   SH(rs1, rs2, asSImm12(ihi, ilo)))
     case 'ihi       rs2 rs1 010 ilo 01000 11' =>  Store(   SW(rs1, rs2, asSImm12(ihi, ilo)))
     case 'ihi       rs2 rs1 011 ilo 01000 11' =>  Store(   SD(rs1, rs2, asSImm12(ihi, ilo)))

     case '_`4 pred succ rs1 000  rd 00011 11' =>        FENCE(rd, rs1, pred, succ)
     case 'imm           rs1 001  rd 00011 11' =>      FENCE_I(rd, rs1, imm)

     case 'csr           rs1 001  rd 11100 11' => System( CSRRW(rd, rs1, csr))
     case 'csr           rs1 010  rd 11100 11' => System( CSRRS(rd, rs1, csr))
     case 'csr           rs1 011  rd 11100 11' => System( CSRRC(rd, rs1, csr))
     case 'csr           rs1 101  rd 11100 11' => System(CSRRWI(rd, rs1, csr))
     case 'csr           rs1 110  rd 11100 11' => System(CSRRSI(rd, rs1, csr))
     case 'csr           rs1 111  rd 11100 11' => System(CSRRCI(rd, rs1, csr))

     case '000000000000  00000 000 00000 11100 11' =>   System( SCALL)
     case '000000000001  00000 000 00000 11100 11' =>   System(SBREAK)
     case '100000000000  00000 000 00000 11100 11' =>   System(  SRET)

     -- unsupported instructions
     case _                                        =>   UnknownInstruction
   }

-- instruction printer

string imm(i::bits(N))  = "0x" : [i]
string instr(o::string) = PadRight(#" ", 7, o)

string pRtype(o::string, rd::reg, rs1::reg, rs2::reg) =
    instr(o) : " " : reg(rd) : ", " : reg(rs1) : ", " : reg(rs2)

string pItype(o::string, rd::reg, rs1::reg, i::bits(N)) =
    instr(o) : " " : reg(rd) : ", " : reg(rs1) : ", " : imm(i)

string pStype(o::string, rs1::reg, rs2::reg, i::bits(N)) =
    instr(o) : " " : reg(rs1) : ", " : reg(rs2) : ", " : imm(i)

string pSBtype(o::string, rs1::reg, rs2::reg, i::bits(N)) =
    instr(o) : " " : reg(rs1) : ", " : reg(rs2) : ", " : imm(i<<1)

string pUtype(o::string, rd::reg, i::bits(N)) =
    instr(o) : " " : reg(rd) : ", " : imm(i)

string pUJtype(o::string, rd::reg, i::bits(N)) =
    instr(o) : " " : reg(rd) : ", " : imm(i<<1)

string pN0type(o::string) =
    instr(o)

string pN1type(o::string, r::reg) =
    instr(o) : " " : reg(r)

string instructionToString(i::instruction) =
   match i
   {
     case Branch(  BEQ(rs1, rs2, imm))      => pSBtype("BEQ",  rs1, rs2, imm)
     case Branch(  BNE(rs1, rs2, imm))      => pSBtype("BNE",  rs1, rs2, imm)
     case Branch(  BLT(rs1, rs2, imm))      => pSBtype("BLT",  rs1, rs2, imm)
     case Branch(  BGE(rs1, rs2, imm))      => pSBtype("BGE",  rs1, rs2, imm)
     case Branch( BLTU(rs1, rs2, imm))      => pSBtype("BLTU", rs1, rs2, imm)
     case Branch( BGEU(rs1, rs2, imm))      => pSBtype("BGEU", rs1, rs2, imm)

     case Branch( JALR(rd, rs1, imm))       => pItype("JALR",  rd, rs1, imm)
     case Branch(  JAL(rd, imm))            => pUJtype("JAL",  rd, imm)

     case ArithI(  LUI(rd, imm))            => pUtype("LUI",   rd, imm)
     case ArithI(AUIPC(rd, imm))            => pUtype("AUIPC", rd, imm)

     case ArithI( ADDI(rd, rs1, imm))       => pItype("ADDI",  rd, rs1, imm)
     case  Shift( SLLI(rd, rs1, imm))       => pItype("SLLI",  rd, rs1, imm)
     case ArithI( SLTI(rd, rs1, imm))       => pItype("SLTI",  rd, rs1, imm)
     case ArithI(SLTIU(rd, rs1, imm))       => pItype("SLTIU", rd, rs1, imm)
     case ArithI( XORI(rd, rs1, imm))       => pItype("XORI",  rd, rs1, imm)
     case  Shift( SRLI(rd, rs1, imm))       => pItype("SRLI",  rd, rs1, imm)
     case  Shift( SRAI(rd, rs1, imm))       => pItype("SRAI",  rd, rs1, imm)
     case ArithI(  ORI(rd, rs1, imm))       => pItype("ORI",   rd, rs1, imm)
     case ArithI( ANDI(rd, rs1, imm))       => pItype("ANDI",  rd, rs1, imm)

     case ArithR(  ADD(rd, rs1, rs2))       => pRtype("ADD",   rd, rs1, rs2)
     case ArithR(  SUB(rd, rs1, rs2))       => pRtype("SUB",   rd, rs1, rs2)
     case  Shift(  SLL(rd, rs1, rs2))       => pRtype("SLL",   rd, rs1, rs2)
     case ArithR(  SLT(rd, rs1, rs2))       => pRtype("SLT",   rd, rs1, rs2)
     case ArithR( SLTU(rd, rs1, rs2))       => pRtype("SLTU",  rd, rs1, rs2)
     case ArithR(  XOR(rd, rs1, rs2))       => pRtype("XOR",   rd, rs1, rs2)
     case  Shift(  SRL(rd, rs1, rs2))       => pRtype("SRL",   rd, rs1, rs2)
     case  Shift(  SRA(rd, rs1, rs2))       => pRtype("SRA",   rd, rs1, rs2)
     case ArithR(   OR(rd, rs1, rs2))       => pRtype("OR",    rd, rs1, rs2)
     case ArithR(  AND(rd, rs1, rs2))       => pRtype("AND",   rd, rs1, rs2)

     case ArithI(ADDIW(rd, rs1, imm))       => pItype("ADDIW", rd, rs1, imm)
     case  Shift(SLLIW(rd, rs1, imm))       => pItype("SLLIW", rd, rs1, imm)
     case  Shift(SRLIW(rd, rs1, imm))       => pItype("SRLIW", rd, rs1, imm)
     case  Shift(SRAIW(rd, rs1, imm))       => pItype("SRAIW", rd, rs1, imm)

     case ArithR( ADDW(rd, rs1, rs2))       => pRtype("ADDW",  rd, rs1, rs2)
     case ArithR( SUBW(rd, rs1, rs2))       => pRtype("SUBW",  rd, rs1, rs2)
     case  Shift( SLLW(rd, rs1, rs2))       => pRtype("SLLW",  rd, rs1, rs2)
     case  Shift( SRLW(rd, rs1, rs2))       => pRtype("SRLW",  rd, rs1, rs2)
     case  Shift( SRAW(rd, rs1, rs2))       => pRtype("SRAW",  rd, rs1, rs2)

     case   Load(   LB(rd, rs1, imm))       => pItype("LB",    rd, rs1, imm)
     case   Load(   LH(rd, rs1, imm))       => pItype("LH",    rd, rs1, imm)
     case   Load(   LW(rd, rs1, imm))       => pItype("LW",    rd, rs1, imm)
     case   Load(   LD(rd, rs1, imm))       => pItype("LD",    rd, rs1, imm)
     case   Load(  LBU(rd, rs1, imm))       => pItype("LBU",   rd, rs1, imm)
     case   Load(  LHU(rd, rs1, imm))       => pItype("LHU",   rd, rs1, imm)
     case   Load(  LWU(rd, rs1, imm))       => pItype("LWU",   rd, rs1, imm)

     case  Store(   SB(rs1, rs2, imm))      => pStype("SB",    rs1, rs2, imm)
     case  Store(   SH(rs1, rs2, imm))      => pStype("SH",    rs1, rs2, imm)
     case  Store(   SW(rs1, rs2, imm))      => pStype("SW",    rs1, rs2, imm)
     case  Store(   SD(rs1, rs2, imm))      => pStype("SD",    rs1, rs2, imm)

     case        FENCE(rd, rs1, pred, succ) => pN0type("FENCE")
     case      FENCE_I(rd, rs1, imm)        => pN0type("FENCE.I")

     case System( SCALL)                    => pN0type("SCALL")
     case System(SBREAK)                    => pN0type("SBREAK")
     case System(  SRET)                    => pN0type("SRET")

     case System( CSRRW(rd, rs1, csr))      => pItype("CSRRW",  rd, rs1, csr)
     case System( CSRRS(rd, rs1, csr))      => pItype("CSRRS",  rd, rs1, csr)
     case System( CSRRC(rd, rs1, csr))      => pItype("CSRRC",  rd, rs1, csr)
     case System(CSRRWI(rd, rs1, csr))      => pItype("CSRRWI", rd, rs1, csr)
     case System(CSRRSI(rd, rs1, csr))      => pItype("CSRRSI", rd, rs1, csr)
     case System(CSRRCI(rd, rs1, csr))      => pItype("CSRRCI", rd, rs1, csr)

     case UnknownInstruction                => pN0type("UNKNOWN")
   }


word Rtype(o::opcode, f3::bits(3), rd::reg, rs1::reg, rs2::reg, f7::bits(7)) =
    f7 : rs2 : rs1 : f3 : rd : o

word Itype(o::opcode, f3::bits(3), rd::reg, rs1::reg, imm::imm12) =
    imm : rs1 : f3 : rd : o

word Stype(o::opcode, f3::bits(3), rs1::reg, rs2::reg, imm::imm12) =
    imm<11:5> : rs2 : rs1 : f3 : imm<4:0> : o

word SBtype(o::opcode, f3::bits(3), rs1::reg, rs2::reg, imm::imm12) =
    [imm<11>]::bits(1) : imm<9:4> : rs2 : rs1 : f3 : imm<3:0> : [imm<10>]::bits(1) : o

word Utype(o::opcode, rd::reg, imm::imm20) =
    imm : rd : o

word UJtype(o::opcode, rd::reg, imm::imm20) =
    [imm<19>]::bits(1) : imm<9:0> : [imm<10>]::bits(1) : imm<18:11> : rd : o

opcode opc(code::bits(8)) = code<4:0> : '11'

word Encode(i::instruction) =
   match i
   {
     case Branch(  BEQ(rs1, rs2, imm))      => SBtype(opc(0x18), 0, rs1, rs2, imm)
     case Branch(  BNE(rs1, rs2, imm))      => SBtype(opc(0x18), 1, rs1, rs2, imm)
     case Branch(  BLT(rs1, rs2, imm))      => SBtype(opc(0x18), 4, rs1, rs2, imm)
     case Branch(  BGE(rs1, rs2, imm))      => SBtype(opc(0x18), 5, rs1, rs2, imm)
     case Branch( BLTU(rs1, rs2, imm))      => SBtype(opc(0x18), 6, rs1, rs2, imm)
     case Branch( BGEU(rs1, rs2, imm))      => SBtype(opc(0x18), 7, rs1, rs2, imm)

     case Branch( JALR(rd, rs1, imm))       =>  Itype(opc(0x19), 0, rd, rs1, imm)
     case Branch(  JAL(rd, imm))            => UJtype(opc(0x1b), rd, imm)

     case ArithI(  LUI(rd, imm))            =>  Utype(opc(0x0D), rd, imm)
     case ArithI(AUIPC(rd, imm))            =>  Utype(opc(0x05), rd, imm)

     case ArithI( ADDI(rd, rs1, imm))       =>  Itype(opc(0x04), 0, rd, rs1, imm)
     case  Shift( SLLI(rd, rs1, imm))       =>  Itype(opc(0x04), 0, rd, rs1, '000000' : imm)
     case ArithI( SLTI(rd, rs1, imm))       =>  Itype(opc(0x04), 2, rd, rs1, imm)
     case ArithI(SLTIU(rd, rs1, imm))       =>  Itype(opc(0x04), 3, rd, rs1, imm)
     case ArithI( XORI(rd, rs1, imm))       =>  Itype(opc(0x04), 4, rd, rs1, imm)
     case  Shift( SRLI(rd, rs1, imm))       =>  Itype(opc(0x04), 5, rd, rs1, '000000' : imm)
     case  Shift( SRAI(rd, rs1, imm))       =>  Itype(opc(0x04), 5, rd, rs1, '010000' : imm)
     case ArithI(  ORI(rd, rs1, imm))       =>  Itype(opc(0x04), 6, rd, rs1, imm)
     case ArithI( ANDI(rd, rs1, imm))       =>  Itype(opc(0x04), 7, rd, rs1, imm)

     case ArithR(  ADD(rd, rs1, rs2))       =>  Rtype(opc(0x0C), 0, rd, rs1, rs2, 0)
     case ArithR(  SUB(rd, rs1, rs2))       =>  Rtype(opc(0x0C), 0, rd, rs1, rs2, 32)
     case  Shift(  SLL(rd, rs1, rs2))       =>  Rtype(opc(0x0C), 1, rd, rs1, rs2, 0)
     case ArithR(  SLT(rd, rs1, rs2))       =>  Rtype(opc(0x0C), 2, rd, rs1, rs2, 0)
     case ArithR( SLTU(rd, rs1, rs2))       =>  Rtype(opc(0x0C), 3, rd, rs1, rs2, 0)
     case ArithR(  XOR(rd, rs1, rs2))       =>  Rtype(opc(0x0C), 4, rd, rs1, rs2, 0)
     case  Shift(  SRL(rd, rs1, rs2))       =>  Rtype(opc(0x0C), 5, rd, rs1, rs2, 0)
     case  Shift(  SRA(rd, rs1, rs2))       =>  Rtype(opc(0x0C), 5, rd, rs1, rs2, 32)
     case ArithR(   OR(rd, rs1, rs2))       =>  Rtype(opc(0x0C), 6, rd, rs1, rs2, 0)
     case ArithR(  AND(rd, rs1, rs2))       =>  Rtype(opc(0x0C), 7, rd, rs1, rs2, 0)

     case ArithI(ADDIW(rd, rs1, imm))       =>  Itype(opc(0x06), 0, rd, rs1, imm)
     case  Shift(SLLIW(rd, rs1, imm))       =>  Itype(opc(0x06), 1, rd, rs1, '0000000' : imm)
     case  Shift(SRLIW(rd, rs1, imm))       =>  Itype(opc(0x06), 5, rd, rs1, '0000000' : imm)
     case  Shift(SRAIW(rd, rs1, imm))       =>  Itype(opc(0x06), 5, rd, rs1, '0100000' : imm)

     case ArithR( ADDW(rd, rs1, rs2))       =>  Rtype(opc(0x0E), 0, rd, rs1, rs2, '0000000')
     case ArithR( SUBW(rd, rs1, rs2))       =>  Rtype(opc(0x0E), 0, rd, rs1, rs2, '0100000')
     case  Shift( SLLW(rd, rs1, rs2))       =>  Rtype(opc(0x0E), 1, rd, rs1, rs2, '0000000')
     case  Shift( SRLW(rd, rs1, rs2))       =>  Rtype(opc(0x0E), 5, rd, rs1, rs2, '0000000')
     case  Shift( SRAW(rd, rs1, rs2))       =>  Rtype(opc(0x0E), 5, rd, rs1, rs2, '0100000')

     case   Load(   LB(rd, rs1, imm))       =>  Itype(opc(0x00), 0, rd, rs1, imm)
     case   Load(   LH(rd, rs1, imm))       =>  Itype(opc(0x00), 1, rd, rs1, imm)
     case   Load(   LW(rd, rs1, imm))       =>  Itype(opc(0x00), 2, rd, rs1, imm)
     case   Load(   LD(rd, rs1, imm))       =>  Itype(opc(0x00), 3, rd, rs1, imm)
     case   Load(  LBU(rd, rs1, imm))       =>  Itype(opc(0x00), 4, rd, rs1, imm)
     case   Load(  LHU(rd, rs1, imm))       =>  Itype(opc(0x00), 5, rd, rs1, imm)
     case   Load(  LWU(rd, rs1, imm))       =>  Itype(opc(0x00), 6, rd, rs1, imm)

     case  Store(   SB(rs1, rs2, imm))      =>  Stype(opc(0x08), 0, rs1, rs2, imm)
     case  Store(   SH(rs1, rs2, imm))      =>  Stype(opc(0x08), 1, rs1, rs2, imm)
     case  Store(   SW(rs1, rs2, imm))      =>  Stype(opc(0x08), 2, rs1, rs2, imm)
     case  Store(   SD(rs1, rs2, imm))      =>  Stype(opc(0x08), 3, rs1, rs2, imm)

     case        FENCE(rd, rs1, pred, succ) =>  Itype(opc(0x03), 0, rd, rs1, '0000' : pred : succ)
     case      FENCE_I(rd, rs1, imm)        =>  Itype(opc(0x03), 1, rd, rs1, imm)

     case System( SCALL)                    =>  Itype(opc(0x1C), 0, 0, 0, 0)
     case System(SBREAK)                    =>  Itype(opc(0x1C), 0, 0, 0, 1)
     case System(  SRET)                    =>  Itype(opc(0x1C), 0, 0, 0, 0x800)

     case System( CSRRW(rd, rs1, csr))      =>  Itype(opc(0x1C), 1, rd, rs1, csr)
     case System( CSRRS(rd, rs1, csr))      =>  Itype(opc(0x1C), 2, rd, rs1, csr)
     case System( CSRRC(rd, rs1, csr))      =>  Itype(opc(0x1C), 3, rd, rs1, csr)
     case System(CSRRWI(rd, rs1, csr))      =>  Itype(opc(0x1C), 5, rd, rs1, csr)
     case System(CSRRSI(rd, rs1, csr))      =>  Itype(opc(0x1C), 6, rd, rs1, csr)
     case System(CSRRCI(rd, rs1, csr))      =>  Itype(opc(0x1C), 7, rd, rs1, csr)

     case UnknownInstruction                => 0
   }


---------------------------------------------------------------------------
-- RISCV memory
---------------------------------------------------------------------------

unit initMem() = VMEM <- InitMap(0x0)

---------------------------------------------------------------------------
-- The next state function
---------------------------------------------------------------------------

string log_instruction(w::word, inst::instruction) =
    "instr " : [procID] : " " : [instCnt] :
    " 0x" : hex64(PC) : " : " : hex32(w) : "   " : instructionToString(inst)

declare done :: bool   -- Flag to request termination

unit Next =
{
    clear_logs();
    match Fetch()
    {
      case Some(w) =>
      {
        inst = Decode(w);
        mark_log(1, log_instruction(w, inst));
        Run(inst)
      }
      case None => nothing
    };
    match Exception, BranchTo
    {
      case None, None       => PC <- PC + 4
      case None, Some(addr) =>
               { BranchTo <- None;
                 PC <- addr
               }
      case Some(_), _       => nothing
    }
}

-- This initializes each core (via setting procID appropriately) on
-- startup before execution begins.
unit initRegs(pc::nat, stack::nat) =
{
   for i in 0 .. 31 do
     gpr([i])   <- 0xAAAAAAAAAAAAAAAA;

   gpr([STACK]) <- [stack];

   PC           <- [pc];
   BranchTo     <- None;
   Exception    <- None;
   done         <- false
}
