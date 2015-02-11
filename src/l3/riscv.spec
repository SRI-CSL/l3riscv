---------------------------------------------------------------------------
--
-- RISC-V Model
-- Based on MIPS specification by Anthony Fox, University of Cambridge
--
---------------------------------------------------------------------------


---------------------------------------------------------------------------
-- Basic types
---------------------------------------------------------------------------

type id     = bits(8)           -- max 256 cores
type reg    = bits(5)

type byte   = bits(8)
type half   = bits(16)
type word   = bits(32)
type dword  = bits(64)

-- instruction fields
type opcode = bits(7)
type imm12  = bits(12)
type imm20  = bits(20)

exception UNPREDICTABLE :: string

-- RV64* base.

type regType  = dword
type vAddr    = dword

---------------------------------------------------------------------------
-- Memory types for Load/Store instructions
---------------------------------------------------------------------------

type memWidth       = bits(3)

memWidth BYTE       = 0
memWidth HALFWORD   = 1
memWidth WORD       = 3
memWidth DOUBLEWORD = 7

---------------------------------------------------------------------------
-- Register state space
---------------------------------------------------------------------------

-- Each register state is local to a core.

type RegFile = reg -> regType

declare
{
  c_gpr         :: id -> RegFile            -- general purpose registers
  c_PC          :: id -> regType            -- program counter
  c_BranchTo    :: id -> regType option     -- requested branch
}

-- Instruction counter
declare instCnt :: nat

-- Current instruction
declare currentInst :: word option

-- Number of cores
declare totalCore :: nat

-- ID of the core executing current instruction
declare procID :: id

-- The following components provide read/write access to state of the
-- core whose id equals procID.  For example, writing "gpr(r)" refers
-- general purpose register "r" in the core whose id equals procID.

component gpr(n::reg) :: regType
{
  value = { m = c_gpr(procID); m(n) }
  assign value = { var m = c_gpr(procID)
                 ; m(n) <- value
                 ; c_gpr(procID) <- m }
}

component PC :: regType
{
  value = c_PC(procID)
  assign value = c_PC(procID) <- value
}

component BranchTo :: regType option
{
   value = c_BranchTo(procID)
   assign value = c_BranchTo(procID) <- value
}

bool NotWordValue(value::regType) =
{
  top = value<63:32>;
  if value<31> then
      top <> 0xFFFF_FFFF
  else
      top <> 0x0
}

---------------------------------------------------------------------------
-- Logging
---------------------------------------------------------------------------

string log_w_gpr(r::reg, data::regType) = "Reg " : [[r]::nat] : " <- 0x" : PadLeft(#"0", 16, [data])

string log_w_mem(addr::vAddr, mask::regType, data::regType) =
    "MEM[0x" : PadLeft(#"0", 10, [addr]) :
    "] <- (data: 0x" : PadLeft(#"0", 16, [data]) :
    ", mask: 0x" : PadLeft(#"0", 16, [mask]) : ")"

string log_r_mem(addr::vAddr, data::regType) =
    "data <- MEM[0x" : PadLeft(#"0", 10, [addr]) :
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

construct ExceptionType
{
  SysCall, SysBreak, ReservedInstr
}

bits(5) ExceptionCode(ExceptionType::ExceptionType) =
{
  0x00
}

unit signalException(ExceptionType::ExceptionType) =
{
  ()
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

unit dumpRegs() =
{
    mark_log(0, "======   Registers   ======")
  ; mark_log(0, "Core = " : [[procID]::nat])
  ; mark_log(0, "PC     " : hex64(PC))
  ; for i in 0 .. 31 do
      mark_log(0, "Reg " : (if i < 10 then " " else "") : [i] : " " :
               hex64(GPR([i])))
}

---------------------------------------------------------------------------
-- Memory access
---------------------------------------------------------------------------

declare VMEM :: vAddr -> regType -- user-space virtual memory

unit initVMEM = VMEM <- InitMap(0x0)

regType readData(vAddr::vAddr) =
{
  data = VMEM(vAddr);
  mark_log(2, log_r_mem(vAddr, data));
  data
}

unit writeData(vAddr::vAddr, data::regType, mask::regType) =
{
  VMEM(vAddr) <- VMEM(vAddr) && ~mask || data && mask;
  mark_log(2, log_w_mem(vAddr, mask, data))
}

word readInst(a::vAddr) = if a<2> then VMEM(a)<31:0> else VMEM(a)<63:32>

-- helper used to preload memory contents
unit writeMem(vAddr::vAddr, data::regType) =
    VMEM(vAddr) <- data

--------------------------------------------------
-- Instruction fetch
--------------------------------------------------

word option Fetch() =
{
  pc = PC;
  Some(readInst(pc))
}

---------------------------------------------------------------------------
-- Integer Computational Instructions
---------------------------------------------------------------------------

-- Integer register-immediate

-----------------------------------
-- ADDI  rd, rs1, imm
-----------------------------------
define ArithI > ADDI(rd::reg, rs1::reg, imm::imm12) =
  GPR(rd) <- GPR(rs1) + SignExtend(imm)

-----------------------------------
-- ADDIW rd, rs1, imm   (RV64I)
-----------------------------------
define ArithI > ADDIW(rd::reg, rs1::reg, imm::imm12) =
{
  temp = GPR(rs1) + SignExtend(imm);
  GPR(rd) <- SignExtend(temp<31:0>)
}

-----------------------------------
-- SLTI  rd, rs1, imm
-----------------------------------
define ArithI > SLTI(rd::reg, rs1::reg, imm::imm12) =
    GPR(rd) <- [GPR(rs1) < SignExtend(imm)]

-----------------------------------
-- SLTIU rd, rs1, imm
-----------------------------------
define ArithI > SLTIU(rd::reg, rs1::reg, imm::imm12) =
    GPR(rd) <- [GPR(rs1) <+ SignExtend(imm)]

-- NOTE: RISCV ANDI/ORI/XORI use sign-extended 12-bit immediates,
-- unlike zero-extended 16-bit immediates in MIPS.

-----------------------------------
-- ANDI  rd, rs1, imm
-----------------------------------
define ArithI > ANDI(rd::reg, rs1::reg, imm::imm12) =
    GPR(rd) <- GPR(rs1) && SignExtend(imm)

-----------------------------------
-- ORI   rd, rs1, imm
-----------------------------------
define ArithI > ORI(rd::reg, rs1::reg, imm::imm12) =
    GPR(rd) <- GPR(rs1) || SignExtend(imm)

-----------------------------------
-- XORI  rd, rs1, imm
-----------------------------------
define ArithI > XORI(rd::reg, rs1::reg, imm::imm12) =
    GPR(rd) <- GPR(rs1) ?? SignExtend(imm)


-- NOTE: RISCV SSLI/SRLI/SRAI use zero-extended 32-bit results, unlike
-- sign-extended results in MIPS.

-----------------------------------
-- SLLI  rd, rs1, imm   (RV32I)
-----------------------------------
define Shift > SLLI32(rd::reg, rs1::reg, imm::bits(5)) =
    GPR(rd) <- GPR(rs1) << [imm]

-----------------------------------
-- SLLI  rd, rs1, imm   (RV64I)
-----------------------------------
define Shift > SLLI(rd::reg, rs1::reg, imm::bits(6)) =
    GPR(rd) <- GPR(rs1) << [imm]

-----------------------------------
-- SRLI  rd, rs1, imm   (RV32I)
-----------------------------------
define Shift > SRLI32(rd::reg, rs1::reg, imm::bits(5)) =
    GPR(rd) <- GPR(rs1) >>+ [imm]

-----------------------------------
-- SRLI  rd, rs1, imm   (RV64I)
-----------------------------------
define Shift > SRLI(rd::reg, rs1::reg, imm::bits(6)) =
    GPR(rd) <- GPR(rs1) >>+ [imm]

-----------------------------------
-- SRAI  rd, rs1, imm   (RV32I)
-----------------------------------
define Shift > SRAI32(rd::reg, rs1::reg, imm::bits(5)) =
    GPR(rd) <- GPR(rs1) >> [imm]

-----------------------------------
-- SRAI  rd, rs1, imm   (RV64I)
-----------------------------------
define Shift > SRAI(rd::reg, rs1::reg, imm::bits(6)) =
    GPR(rd) <- GPR(rs1) >> [imm]

-----------------------------------
-- SLLIW rd, rs1, imm   (RV64I)
-----------------------------------
define Shift > SLLIW(rd::reg, rs1::reg, imm::bits(5)) =
{
  when NotWordValue(GPR(rs1)) do #UNPREDICTABLE("SLLIW: NotWordValue");
  GPR(rd) <- SignExtend(GPR(rs1)<31:0> << [imm])
}

-----------------------------------
-- SRLIW rd, rs1, imm   (RV64I)
-----------------------------------
define Shift > SRLIW(rd::reg, rs1::reg, imm::bits(5)) =
{
  when NotWordValue(GPR(rs1)) do #UNPREDICTABLE("SRLIW: NotWordValue");
  GPR(rd) <- SignExtend(GPR(rs1)<31:0> >>+ [imm])
}

-----------------------------------
-- SRAIW rd, rs1, imm   (RV64I)
-----------------------------------
define Shift > SRAIW(rd::reg, rs1::reg, imm::bits(5)) =
{
  when NotWordValue(GPR(rs1)) do #UNPREDICTABLE("SRAIW: NotWordValue");
  GPR(rd) <- SignExtend(GPR(rs1)<31:0> >> [imm])
}

-----------------------------------
-- LUI   rd, imm
-----------------------------------
define ArithI > LUI(rd::reg, imm::bits(20)) =
   GPR(rd) <- SignExtend(imm : 0`12)

-----------------------------------
-- AUIPC rd, imm
-----------------------------------
-- ISSUE: the descriptions for RV32I and RV64I are not compatible.
define ArithI > AUIPC(rd::reg, imm::bits(20)) =
{
  temp = SignExtend(imm : 0`12);
  GPR(rd) <- PC + temp
}

-- Integer register-register

-----------------------------------
-- ADD   rd, rs1, rs2
-----------------------------------
define ArithR > ADD(rd::reg, rs1::reg, rs2::reg) =
    GPR(rd) <- GPR(rs1) + GPR(rs2)

-----------------------------------
-- ADDW  rd, rs1, rs2   (RV64I)
-----------------------------------
define ArithR > ADDW(rd::reg, rs1::reg, rs2::reg) =
    GPR(rd) <- SignExtend(GPR(rs1)<31:0> + GPR(rs2)<31:0>)

-----------------------------------
-- SUB   rd, rs1, rs2
-----------------------------------
define ArithR > SUB(rd::reg, rs1::reg, rs2::reg) =
    GPR(rd) <- GPR(rs1) - GPR(rs2)

-----------------------------------
-- SUBW  rd, rs1, rs2   (RV64I)
-----------------------------------
define ArithR > SUBW(rd::reg, rs1::reg, rs2::reg) =
    GPR(rd) <- SignExtend(GPR(rs1)<31:0> - GPR(rs2)<31:0>)

-----------------------------------
-- SLT   rd, rs1, rs2
-----------------------------------
define ArithR > SLT(rd::reg, rs1::reg, rs2::reg) =
   GPR(rd) <- [GPR(rs1) < GPR(rs2)]

-----------------------------------
-- SLTU  rd, rs1, rs2
-----------------------------------
define ArithR > SLTU(rd::reg, rs1::reg, rs2::reg) =
   GPR(rd) <- [GPR(rs1) <+ GPR(rs2)]

-----------------------------------
-- AND   rd, rs1, rs2
-----------------------------------
define ArithR > AND(rd::reg, rs1::reg, rs2::reg) =
   GPR(rd) <- GPR(rs1) && GPR(rs2)

-----------------------------------
-- OR    rd, rs1, rs2
-----------------------------------
define ArithR > OR(rd::reg, rs1::reg, rs2::reg) =
   GPR(rd) <- GPR(rs1) || GPR(rs2)

-----------------------------------
-- XOR   rd, rs1, rs2
-----------------------------------
define ArithR > XOR(rd::reg, rs1::reg, rs2::reg) =
   GPR(rd) <- GPR(rs1) ?? GPR(rs2)

-----------------------------------
-- SLL   rd, rs1, rs2   (RV32I)
-----------------------------------
define Shift > SLL32(rd::reg, rs1::reg, rs2::reg) =
    GPR(rd) <- GPR(rs1) << ZeroExtend(GPR(rs2)<4:0>)

-----------------------------------
-- SLL   rd, rs1, rs2   (RV64I)
-----------------------------------
define Shift > SLL(rd::reg, rs1::reg, rs2::reg) =
    GPR(rd) <- GPR(rs1) << ZeroExtend(GPR(rs2)<5:0>)

-----------------------------------
-- SLLW  rd, rs1, rs2   (RV64I)
-----------------------------------
define Shift > SLLW(rd::reg, rs1::reg, rs2::reg) =
    GPR(rd) <- SignExtend(GPR(rs1)<31:0> << ZeroExtend(GPR(rs2)<4:0>))

-----------------------------------
-- SRL   rd, rs1, rs2   (RV32I)
-----------------------------------
define Shift > SRL32(rd::reg, rs1::reg, rs2::reg) =
    GPR(rd) <- GPR(rs1) >>+ ZeroExtend(GPR(rs2)<4:0>)

-----------------------------------
-- SRL   rd, rs1, rs2   (RV64I)
-----------------------------------
define Shift > SRL(rd::reg, rs1::reg, rs2::reg) =
    GPR(rd) <- GPR(rs1) >>+ ZeroExtend(GPR(rs2)<5:0>)

-----------------------------------
-- SRLW  rd, rs1, rs2   (RV64I)
-----------------------------------
define Shift > SRLW(rd::reg, rs1::reg, rs2::reg) =
    GPR(rd) <- SignExtend(GPR(rs1)<31:0> >>+ ZeroExtend(GPR(rs2)<4:0>))

-----------------------------------
-- SRA   rd, rs1, rs2   (RV32I)
-----------------------------------
define Shift > SRA32(rd::reg, rs1::reg, rs2::reg) =
    GPR(rd) <- GPR(rs1) >> ZeroExtend(GPR(rs2)<4:0>)

-----------------------------------
-- SRA   rd, rs1, rs2   (RV64I)
-----------------------------------
define Shift > SRA(rd::reg, rs1::reg, rs2::reg) =
    GPR(rd) <- GPR(rs1) >> ZeroExtend(GPR(rs2)<5:0>)

-----------------------------------
-- SRAW  rd, rs1, rs2   (RV64I)
-----------------------------------
define Shift > SRAW(rd::reg, rs1::reg, rs2::reg) =
    GPR(rd) <- SignExtend(GPR(rs1)<31:0> >> ZeroExtend(GPR(rs2)<4:0>))

---------------------------------------------------------------------------
-- Control Transfer Instructions
---------------------------------------------------------------------------

-- Unconditional jumps

-----------------------------------
-- JAL   rd, offs
-----------------------------------
define Branch > JAL(rd::reg, offs::bits(20)) =
{
   GPR(rd)  <- PC + 4;
   BranchTo <- Some(PC + SignExtend(offs << 1))
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
      BranchTo <- Some(PC + (SignExtend(offs) << 2))
   else
      ()

-----------------------------------
-- BNE   rs1, rs2, offs
-----------------------------------
define Branch > BNE(rs1::reg, rs2::reg, offs::imm12) =
   if GPR(rs1) <> GPR(rs2) then
      BranchTo <- Some(PC + (SignExtend(offs) << 2))
   else
      ()

-----------------------------------
-- BLT   rs1, rs2, offs
-----------------------------------
define Branch > BLT(rs1::reg, rs2::reg, offs::imm12) =
   if GPR(rs1) < GPR(rs2) then
      BranchTo <- Some(PC + (SignExtend(offs) << 2))
   else
      ()

-----------------------------------
-- BLTU  rs1, rs2, offs
-----------------------------------
define Branch > BLTU(rs1::reg, rs2::reg, offs::imm12) =
   if GPR(rs1) <+ GPR(rs2) then
      BranchTo <- Some(PC + (SignExtend(offs) << 2))
   else
      ()

-----------------------------------
-- BGE   rs1, rs2, offs
-----------------------------------
define Branch > BGE(rs1::reg, rs2::reg, offs::imm12) =
   if GPR(rs1) >= GPR(rs2) then
      BranchTo <- Some(PC + (SignExtend(offs) << 2))
   else
      ()

-----------------------------------
-- BGEU  rs1, rs2, offs
-----------------------------------
define Branch > BGEU(rs1::reg, rs2::reg, offs::imm12) =
   if GPR(rs1) >=+ GPR(rs2) then
      BranchTo <- Some(PC + (SignExtend(offs) << 2))
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
  addr = GPR(rs1) + SignExtend(offs);
  val  = readData(addr);
  GPR(rd) <- ZeroExtend(val<31:0>)
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
{
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
  writeData(addr, GPR(rs2), mask)
}

-----------------------------------
-- SH    rs1, rs2, offs
-----------------------------------
define Store > SH(rs1::reg, rs2::reg, offs::imm12) =
{
  addr = GPR(rs1) + SignExtend(offs);
  mask = 0xFFFF;
  writeData(addr, GPR(rs2), mask)
}

-----------------------------------
-- SB    rs1, rs2, offs
-----------------------------------
define Store > SB(rs1::reg, rs2::reg, offs::imm12) =
{
  addr = GPR(rs1) + SignExtend(offs);
  mask = 0xFF;
  writeData(addr, GPR(rs2), mask)
}

-----------------------------------
-- SD    rs1, rs2, offs (RV64I)
-----------------------------------
define Store > SD(rs1::reg, rs2::reg, offs::imm12) =
{
  addr = GPR(rs1) + SignExtend(offs);
  writeData(addr, GPR(rs2), SignExtend('1'))
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
define System > SCALL  = signalException(SysCall)

-----------------------------------
-- SBREAK
-----------------------------------
define System > SBREAK = signalException(SysBreak)

-- Timers and Counters

-----------------------------------
-- CSRRW  rd, rs1, imm
-----------------------------------
define System > CSRRW(rd::reg, rs1::reg, imm::imm12) = nothing

-----------------------------------
-- CSRRS  rd, rs1, imm
-----------------------------------
define System > CSRRS(rd::reg, rs1::reg, imm::imm12) = nothing

-----------------------------------
-- CSRRC  rd, rs1, imm
-----------------------------------
define System > CSRRC(rd::reg, rs1::reg, imm::imm12) = nothing

-----------------------------------
-- CSRRWI rd, rs1, imm
-----------------------------------
define System > CSRRWI(rd::reg, rs1::reg, imm::imm12) = nothing

-----------------------------------
-- CSRRSI rd, rs1, imm
-----------------------------------
define System > CSRRSI(rd::reg, rs1::reg, imm::imm12) = nothing

-----------------------------------
-- CSRRCI rd, rs1, imm
-----------------------------------
define System > CSRRCI(rd::reg, rs1::reg, imm::imm12) = nothing

-----------------------------------
-- Reserved instruction (for unsuccessful decode)
-----------------------------------
define ReservedInstruction =
   signalException(ReservedInstr)

define Unpredictable = #UNPREDICTABLE("Unpredictable instruction")

define Run

---------------------------------------------------------------------------
-- Instruction decoding
---------------------------------------------------------------------------

-- string utilities
string r  (n::reg)       = "$" : [[n]::nat]
string c  (n::reg)       = ", " : r(n)
string i  (n::bits(N))   = ", " : (if n <+ 10 then "" else "0x") : [n]
string oi (n::bits(N))   = if n == 0 then "" else i(n)

-- helper to assemble various immediates from their pieces
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

     case 'imm           rs1 001  rd 11100 11' => System( CSRRW(rd, rs1, imm))
     case 'imm           rs1 010  rd 11100 11' => System( CSRRS(rd, rs1, imm))
     case 'imm           rs1 011  rd 11100 11' => System( CSRRC(rd, rs1, imm))
     case 'imm           rs1 101  rd 11100 11' => System(CSRRWI(rd, rs1, imm))
     case 'imm           rs1 110  rd 11100 11' => System(CSRRSI(rd, rs1, imm))
     case 'imm           rs1 111  rd 11100 11' => System(CSRRCI(rd, rs1, imm))

     case '000000000000  00000 000 00000 11100 11' =>   System( SCALL)
     case '000000000001  00000 000 00000 11100 11' =>   System(SBREAK)

     -- reserved instructions
     case _                                        => ReservedInstruction
   }

-- instruction to string

string instructionToString(i::instruction) =
   match i
   {
     case ArithI(ADDI(rd, rs1, imm))     => "addi"

     case Unpredictable                  => "???"
     case ReservedInstruction            => "???"
     case _                              => "???"
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

     case System( CSRRW(rd, rs1, imm))      =>  Itype(opc(0x1C), 1, rd, rs1, imm)
     case System( CSRRS(rd, rs1, imm))      =>  Itype(opc(0x1C), 2, rd, rs1, imm)
     case System( CSRRC(rd, rs1, imm))      =>  Itype(opc(0x1C), 3, rd, rs1, imm)
     case System(CSRRWI(rd, rs1, imm))      =>  Itype(opc(0x1C), 5, rd, rs1, imm)
     case System(CSRRSI(rd, rs1, imm))      =>  Itype(opc(0x1C), 6, rd, rs1, imm)
     case System(CSRRCI(rd, rs1, imm))      =>  Itype(opc(0x1C), 7, rd, rs1, imm)

     case Unpredictable                  => '00000111111100000000000000000000'
     case ReservedInstruction            => 0
   }


---------------------------------------------------------------------------
-- RISCV memory
---------------------------------------------------------------------------

unit InitMEM = VMEM <- InitMap(0x0)

---------------------------------------------------------------------------
-- The next state function
---------------------------------------------------------------------------

string log_instruction(w::word, inst::instruction) =
    "instr " : [procID] : " " : [instCnt] : " " :
    hex64(PC) : " : " : hex32(w) : "   " : instructionToString(inst)

declare done :: bool   -- Flag to request termination

unit Next =
{
    clear_logs();
    currentInst <- None;
    currentInst <- Fetch();
    match currentInst
    {
        case Some(w) =>
        {
            inst = Decode(w);
            mark_log(1, log_instruction(w,inst));
            Run(inst)
        }
        case None => nothing
    };
    match BranchTo
    {
        case None           => PC <- PC + 4
        case Some(addr)     =>
                 { BranchTo <- None;
                   PC <- addr
                 }
    }
}

-- This initializes each core (via setting procID appropriately) on
-- startup before execution begins.
unit initRISCV(pc::nat) =
{
   BranchTo <- None;
   PC <- [pc];
   done <- false;
   for i in 0 .. 31 do gpr([i]) <- 0xAAAAAAAAAAAAAAAA;
   when procID == 0 do
     {
       InitMEM
     }
}
