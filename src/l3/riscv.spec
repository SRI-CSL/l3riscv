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
  System,  Break, ReservedInstr
}

bits(5) ExceptionCode(ExceptionType::ExceptionType) =
{
  0x00
}

unit SignalException(ExceptionType::ExceptionType) =
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
define ArithI > ADDIW(rd::reg, rs1::reg, imm::bits(12)) =
{
  temp = GPR(rs1) + SignExtend(imm);
  GPR(rd) <- SignExtend(temp<31:0>)
}

-----------------------------------
-- SLTI  rd, rs1, imm
-----------------------------------
define ArithI > SLTI(rd::reg, rs1::reg, imm::bits(12)) =
    GPR(rd) <- [GPR(rs1) < SignExtend(imm)]

-----------------------------------
-- SLTIU rd, rs1, imm
-----------------------------------
define ArithI > SLTIU(rs1::reg, rd::reg, imm::bits(12)) =
    GPR(rd) <- [GPR(rs1) <+ SignExtend(imm)]

-- NOTE: RISCV ANDI/ORI/XORI use sign-extended 12-bit immediates,
-- unlike zero-extended 16-bit immediates in MIPS.

-----------------------------------
-- ANDI  rd, rs1, imm
-----------------------------------
define ArithI > ANDI(rd::reg, rs1::reg, imm::bits(12)) =
    GPR(rd) <- GPR(rs1) && SignExtend(imm)

-----------------------------------
-- ORI   rd, rs1, imm
-----------------------------------
define ArithI > ORI(rd::reg, rs1::reg, imm::bits(12)) =
    GPR(rd) <- GPR(rs1) || SignExtend(imm)

-----------------------------------
-- XORI  rd, rs1, imm
-----------------------------------
define ArithI > XORI(rd::reg, rs1::reg, imm::bits(12)) =
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
define Branch > JALR(rd::reg, rs1::reg, imm::bits(12)) =
{
  temp      = GPR(rs1) + SignExtend(imm);
  GPR(rd)  <- PC + 4;
  BranchTo <- Some(temp && SignExtend('10'))
}

-- conditional branches

-----------------------------------
-- BEQ   rs1, rs2, offs
-----------------------------------
define Branch > BEQ(rs1::reg, rs2::reg, offs::bits(12)) =
   if GPR(rs1) == GPR(rs2) then
      BranchTo <- Some(PC + (SignExtend(offs) << 2))
   else
      ()

-----------------------------------
-- BNE   rs1, rs2, offs
-----------------------------------
define Branch > BNE(rs1::reg, rs2::reg, offs::bits(12)) =
   if GPR(rs1) <> GPR(rs2) then
      BranchTo <- Some(PC + (SignExtend(offs) << 2))
   else
      ()

-----------------------------------
-- BLT   rs1, rs2, offs
-----------------------------------
define Branch > BLT(rs1::reg, rs2::reg, offs::bits(12)) =
   if GPR(rs1) < GPR(rs2) then
      BranchTo <- Some(PC + (SignExtend(offs) << 2))
   else
      ()

-----------------------------------
-- BLTU  rs1, rs2, offs
-----------------------------------
define Branch > BLTU(rs1::reg, rs2::reg, offs::bits(12)) =
   if GPR(rs1) <+ GPR(rs2) then
      BranchTo <- Some(PC + (SignExtend(offs) << 2))
   else
      ()

-----------------------------------
-- BGE   rs1, rs2, offs
-----------------------------------
define Branch > BGE(rs1::reg, rs2::reg, offs::bits(12)) =
   if GPR(rs1) >= GPR(rs2) then
      BranchTo <- Some(PC + (SignExtend(offs) << 2))
   else
      ()

-----------------------------------
-- BGEU  rs1, rs2, offs
-----------------------------------
define Branch > BGEU(rs1::reg, rs2::reg, offs::bits(12)) =
   if GPR(rs1) >=+ GPR(rs2) then
      BranchTo <- Some(PC + (SignExtend(offs) << 2))
   else
      ()

---------------------------------------------------------------------------
-- Load and Store Instructions
---------------------------------------------------------------------------

-----------------------------------
-- LW    rd, offs
-----------------------------------
define Load > LW(rd::reg, rs1::reg, offs::bits(12)) =
{
  addr = GPR(rs1) + SignExtend(offs);
  val  = readData(addr);
  GPR(rd) <- SignExtend(val<31:0>)
}

-----------------------------------
-- LWU   rd, offs       (RV64I)
-----------------------------------
define Load > LWU(rd::reg, rs1::reg, offs::bits(12)) =
{
  addr = GPR(rs1) + SignExtend(offs);
  val  = readData(addr);
  GPR(rd) <- ZeroExtend(val<31:0>)
}

-----------------------------------
-- LH    rd, offs
-----------------------------------
define Load > LH(rd::reg, rs1::reg, offs::bits(12)) =
{
  addr = GPR(rs1) + SignExtend(offs);
  val  = readData(addr);
  GPR(rd) <- SignExtend(val<15:0>)
}

-----------------------------------
-- LHU   rd, offs
-----------------------------------
define Load > LHU(rd::reg, rs1::reg, offs::bits(12)) =
{
  addr = GPR(rs1) + SignExtend(offs);
  val  = readData(addr);
  GPR(rd) <- ZeroExtend(val<15:0>)
}

-----------------------------------
-- LB    rd, offs
-----------------------------------
define Load > LB(rd::reg, rs1::reg, offs::bits(12)) =
{
  addr = GPR(rs1) + SignExtend(offs);
  val  = readData(addr);
  GPR(rd) <- SignExtend(val<7:0>)
}

-----------------------------------
-- LBU   rd, offs
-----------------------------------
define Load > LBU(rd::reg, rs1::reg, offs::bits(12)) =
{
  addr = GPR(rs1) + SignExtend(offs);
  val  = readData(addr);
  GPR(rd) <- ZeroExtend(val<7:0>)
}

-----------------------------------
-- SW    rs1, rs2, offs
-----------------------------------
define Store > SW(rs1::reg, rs2::reg, offs::bits(12)) =
{
  addr = GPR(rs1) + SignExtend(offs);
  mask = 0xFFFF_FFFF;
  writeData(addr, GPR(rs2), mask)
}

-----------------------------------
-- SH    rs1, rs2, offs
-----------------------------------
define Store > SH(rs1::reg, rs2::reg, offs::bits(12)) =
{
  addr = GPR(rs1) + SignExtend(offs);
  mask = 0xFFFF;
  writeData(addr, GPR(rs2), mask)
}

-----------------------------------
-- SB    rs1, rs2, offs
-----------------------------------
define Store > SB(rs1::reg, rs2::reg, offs::bits(12)) =
{
  addr = GPR(rs1) + SignExtend(offs);
  mask = 0xFF;
  writeData(addr, GPR(rs2), mask)
}

-----------------------------------
-- SD    rs1, rs2, offs (RV64I)
-----------------------------------
define Store > SD(rs1::reg, rs2::reg, offs::bits(12)) =
{
  addr = GPR(rs1) + SignExtend(offs);
  writeData(addr, GPR(rs2), SignExtend('1'))
}

---------------------------------------------------------------------------
-- Memory model
---------------------------------------------------------------------------

-----------------------------------
-- FENCE rd, rs1, pre, succ
-----------------------------------
define FENCE(rd::reg, rs1::reg, pre::bits(4), succ::bits(4)) = nothing

---------------------------------------------------------------------------
-- System Instructions
---------------------------------------------------------------------------

-----------------------------------
-- SCALL
-----------------------------------
define SCALL  = SignalException(System)

-----------------------------------
-- SBREAK
-----------------------------------
define SBREAK = SignalException(Break)

-----------------------------------
-- Reserved instruction (for unsuccessful decode)
-----------------------------------
define ReservedInstruction =
   SignalException(ReservedInstr)

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



instruction Decode(w::word) =
   match w
   {
     case 'imm rs1 000 rd 00100 11' => ArithI(ADDI(rd, rs1, imm))

     -- reserved instructions
     case _ => ReservedInstruction
   }

-- instruction to string

string instructionToString(i::instruction) =
   match i
   {
     case ArithI(ADDI(rd, rs1, imm))   => "addi"

     case Unpredictable                  => "???"
     case ReservedInstruction            => "???"
   }

word Encode(i::instruction) =
   match i
   {
     case ArithI(ADDI(rs, rt, imm))      => '001000' : '00000000000000000000000000'

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
        case Some(addr)    =>
                 { BranchTo <- None;
                   PC <- addr
                 }
        case _              => #UNPREDICTABLE("Branch follows branch")
    }
}

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
