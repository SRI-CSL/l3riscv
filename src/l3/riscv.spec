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

type exc_code = bits(4)

-- instruction fields
type opcode   = bits(7)
type imm12    = bits(12)
type imm20    = bits(20)
type amo      = bits(1)

construct accessType { Read, Write }
construct fetchType  { Instruction, Data }

-- RV64* base.

type regType  = dword
type vAddr    = dword
type pAddr    = bits(61)        -- internal accesses are 8-byte aligned

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

type arch_base = bits(2)

construct Architecture
{
  RV32I, RV64I, RV128I
}

arch_base archBase(a::Architecture) =
    match a
    { case RV32I      => 0
      case RV64I      => 2
      case RV128I     => 3
    }

Architecture architecture(ab::arch_base) =
    match ab
    { case 0          => RV32I
      case 2          => RV64I
      case 3          => RV128I
    }

string archName(a::Architecture) =
    match a
    { case RV32I      => "RV32I"
      case RV64I      => "RV64I"
      case RV128I     => "RV128I"
    }

---------------------------------------------------------------------------
-- Privilege levels
---------------------------------------------------------------------------

type priv_level = bits(2)

construct Privilege
{ User
, Supervisor
, Hypervisor
, Machine
}

priv_level privLevel(p::Privilege) =
    match p
    { case User       => 0
      case Supervisor => 1
      case Hypervisor => 2
      case Machine    => 3
    }

Privilege privilege(p::priv_level) =
    match p
    { case 0          => User
      case 1          => Supervisor
      case 2          => Hypervisor
      case 3          => Machine
    }

string privName(p::Privilege) =
    match p
    { case User       => "U"
      case Supervisor => "S"
      case Hypervisor => "H"
      case Machine    => "M"
    }

---------------------------------------------------------------------------
-- Memory management and virtualization
---------------------------------------------------------------------------

type vm_mode    = bits(5)

construct VM_Mode
{ Mbare
, Mbb
, Mbbid
, Sv32
, Sv39
, Sv48
, Sv57
, Sv64
}

vm_mode vmMode(vm::VM_Mode) =
    match vm
    { case Mbare  => 0
      case Mbb    => 1
      case Mbbid  => 2
      case Sv32   => 8
      case Sv39   => 9
      case Sv48   => 10
      case Sv57   => 11
      case Sv64   => 12
    }

string vmModeName(vm::VM_Mode) =
    match vm
    { case Mbare  => "Mbare"
      case Mbb    => "Mbb"
      case Mbbid  => "Mbbid"
      case Sv32   => "Sv32"
      case Sv39   => "Sv39"
      case Sv48   => "Sv48"
      case Sv57   => "Sv57"
      case Sv64   => "Sv64"
    }

---------------------------------------------------------------------------
-- Extension Context Status
---------------------------------------------------------------------------

type ext_status = bits(2)

construct ExtStatus
{ Off
, Initial
, Clean
, Dirty
}

ext_status extStatus(e::ExtStatus) =
    match e
    { case Off      => 0
      case Initial  => 1
      case Clean    => 2
      case Dirty    => 3
    }

string extStatusName(e::ExtStatus) =
    match e
    { case Off      => "Off"
      case Initial  => "Initial"
      case Clean    => "Clean"
      case Dirty    => "Dirty"
    }

---------------------------------------------------------------------------
-- Exceptions and Interrupts
---------------------------------------------------------------------------

construct Interrupt
{ Software
, Timer
}

exc_code interruptIndex(i::Interrupt) =
    match i
    { case Software     => 0
      case Timer        => 1
    }

construct ExceptionType
{ Fetch_Misaligned
, Fetch_Fault
, Illegal_Instr
, Breakpoint
, Load_Misaligned
, Load_Fault
, Store_Misaligned
, Store_Fault
, UMode_Env_Call
, SMode_Env_Call
, HMode_Env_Call
, MMode_Env_Call
}

exc_code excCode(e::ExceptionType) =
    match e
    { case Fetch_Misaligned   => 0x0
      case Fetch_Fault        => 0x1
      case Illegal_Instr      => 0x2
      case Breakpoint         => 0x3

      case Load_Misaligned    => 0x4
      case Load_Fault         => 0x5
      case Store_Misaligned   => 0x6
      case Store_Fault        => 0x7

      case UMode_Env_Call     => 0x8
      case SMode_Env_Call     => 0x9
      case HMode_Env_Call     => 0xA
      case MMode_Env_Call     => 0xB
    }

ExceptionType excType(e::exc_code) =
    match e
    { case 0x0 => Fetch_Misaligned
      case 0x1 => Fetch_Fault
      case 0x2 => Illegal_Instr
      case 0x3 => Breakpoint

      case 0x4 => Load_Misaligned
      case 0x5 => Load_Fault
      case 0x6 => Store_Misaligned
      case 0x7 => Store_Fault

      case 0x8 => UMode_Env_Call
      case 0x9 => SMode_Env_Call
      case 0xA => HMode_Env_Call
      case 0xB => MMode_Env_Call

      case _   => #UNDEFINED("Unknown exception")
    }

string excName(e::ExceptionType) =
    match e
    { case Fetch_Misaligned   => "MISALIGNED_FETCH"
      case Fetch_Fault        => "FAULT_FETCH"
      case Illegal_Instr      => "ILLEGAL_INSTRUCTION"
      case Breakpoint         => "BREAKPOINT"

      case Load_Misaligned    => "MISALIGNED_LOAD"
      case Load_Fault         => "FAULT_LOAD"
      case Store_Misaligned   => "MISALIGNED_STORE"
      case Store_Fault        => "FAULT_STORE"

      case UMode_Env_Call     => "U-EnvCall"
      case SMode_Env_Call     => "S-EnvCall"
      case HMode_Env_Call     => "H-EnvCall"
      case MMode_Env_Call     => "M-EnvCall"
    }

regType makeExceptionCause(e::ExceptionType) =
    [excCode(e)]

bool isBadAddressException(e::ExceptionType) =
    match e
    { case Load_Misaligned
      or   Store_Misaligned
      or   Load_Fault
      or   Store_Fault => true
      case _           => false
    }

---------------------------------------------------------------------------
-- Control and Status Registers (CSRs)
---------------------------------------------------------------------------

-- Machine-Level CSRs

register mcpuid :: regType
{ 63-62 : ArchBase  -- base architecture, machine mode on reset
,    20 : U         -- user-mode support
,    18 : S         -- supervisor-mode support
,    12 : M         -- integer multiply/divide support
,     8 : I         -- integer base ISA support (XXX: this seems unnecessary)
}

register mimpid :: regType
{ 63-16 : RVImpl
,  15-0 : RVSource
}

register mstatus :: regType
{    63 : MSD       -- extended context dirty status
, 21-17 : VM        -- memory management and virtualization
,    16 : MMPRV     -- load/store memory privilege
, 15-14 : MXS       -- extension context status
, 13-12 : MFS       -- floating-point context status
            -- privilege and global interrupt-enable stack
, 11-10 : MPRV3
,     9 : MIE3
,   8-7 : MPRV2
,     6 : MIE2
,   5-4 : MPRV1
,     3 : MIE1
,   2-1 : MPRV
,     0 : MIE
}

register mtdeleg :: regType
{ 63-16 : Intr_deleg
, 15-0  : Exc_deleg
}

register mip :: regType
{           -- pending timer interrupts (read-only)
      7 : MTIP
,     6 : HTIP
,     5 : STIP
            -- pending software interrupts (read/write)
,     3 : MSIP
,     2 : HSIP
,     1 : SSIP
}

register mie :: regType
{           -- enable timer interrupts (read-only)
      7 : MTIE
,     6 : HTIE
,     5 : STIE
            -- enable software interrupts (read/write)
,     3 : MSIE
,     2 : HSIE
,     1 : SSIE
}

register mcause :: regType
{    63 : Int   -- Interrupt
    3-0 : EC    -- Exception Code
}

record MachineCSR
{ mcpuid        :: mcpuid       -- information registers
  mimpid        :: mimpid
  mhartid       :: regType

  mstatus       :: mstatus      -- trap setup
  mtvec         :: regType
  mtdeleg       :: mtdeleg
  mie           :: mie
  mtimecmp      :: regType

  mtime         :: regType      -- timers and counters

  mscratch      :: regType      -- trap handling
  mepc          :: regType
  mcause        :: mcause
  mbadaddr      :: regType
  mip           :: mip

  mbase         :: regType      -- protection and translation
  mbound        :: regType
  mibase        :: regType
  mibound       :: regType
  mdbase        :: regType
  mdbound       :: regType

                   -- host-target interface (berkeley extensions)
  mtohost       :: regType      -- output register to host
  mfromhost     :: regType      -- input register from host
}

-- Hypervisor-Level CSRs

record HypervisorCSR
{ hstatus       :: mstatus      -- trap setup
  htvec         :: regType
  htdeleg       :: mtdeleg
  htimecmp      :: regType

  htime         :: regType      -- timer

  hscratch      :: regType      -- trap handling
  hepc          :: regType
  hcause        :: mcause
  hbadaddr      :: regType
}

-- Supervisor-Level CSRs

register sstatus :: regType
{    63 : SSD       -- extended context dirty status
,    16 : SMPRV     -- load/store memory privilege
, 15-14 : SXS       -- extension context status
, 13-12 : SFS       -- floating-point context status
,     4 : SPS       -- previous privilege level before entering supervisor mode
,     3 : SPIE      -- interrupt-enable before entering supervisor mode
,     0 : SIE       -- supervisor-level interrupt-enable
}

register sip :: regType
{     5 : STIP      -- pending timer interrupt
,     1 : SSIP      -- pending software interrupt
}

register sie :: regType
{     5 : STIE      -- enable timer interrupt
,     1 : SSIE      -- enable software interrupt
}

record SupervisorCSR
{ sstatus       :: sstatus      -- trap setup
  stvec         :: regType
  -- sie :: sie is a projection of mie :: mie
  stimecmp      :: regType

  stime         :: regType      -- timers and counters

  sscratch      :: regType      -- trap handling
  sepc          :: regType
  scause        :: mcause
  sbadaddr      :: regType
  -- sip :: sip is a projection of mip :: mip

  sptbr         :: regType      -- memory protection and translation
  sasid         :: regType
}

-- User-Level CSRs

record UserCSR
{ cycle         :: regType
  time          :: regType
  instret       :: regType
}

-- Machine state projections

sip lift_mip_sip(mip::mip) =
{ var sip = sip(0)
; sip.STIP  <- mip.STIP
; sip.SSIP  <- mip.SSIP
; sip
}

sie lift_mie_sie(mie::mie) =
{ var sie = sie(0)
; sie.STIE  <- mie.STIE
; sie.SSIE  <- mie.SSIE
; sie
}

mip lower_sip_mip(sip::sip, mip::mip) =
{ var m = mip
; m.STIP    <- sip.STIP
; m.SSIP    <- sip.SSIP
; m
}

mie lower_sie_mie(sie::sie, mie::mie) =
{ var m = mie
; m.STIE    <- sie.STIE
; m.SSIE    <- sie.SSIE
; m
}

-- We need both status registers here since some bits of status are
-- local to S-mode, while others are projections.
sstatus lift_mstatus_sstatus(mst::mstatus, sst::sstatus) =
{ var st = sstatus(0)
-- local state
; st.SMPRV  <- sst.SMPRV

-- shared state
; st.SSD    <- mst.MSD
; st.SXS    <- mst.MXS
; st.SFS    <- mst.MFS

-- projected state
; st.SPS    <- not (privilege(mst.MPRV1) == User)
; st.SPIE   <- mst.MIE1

-- conditional projections
; st.SIE    <- if privilege(mst.MPRV) == Supervisor
               then mst.MIE else sst.SIE

; st
}

-- A write to sstatus results in updates to both sstatus as well as
-- mstatus, so this function returns the updated versions of both
-- registers.
sstatus * mstatus lower_sstatus_mstatus(new_sst::sstatus, old_sst::sstatus,
                                        mst::mstatus) =
{ var mt = mstatus(&mst)
; var st = sstatus(&new_sst)

-- shared state
; mt.MSD    <- st.SSD
; mt.MXS    <- st.SXS
; mt.MFS    <- st.SFS

-- back-projected state
; chk = sstatus (&new_sst ?? &old_sst)
; when chk.SPS do
   mt.MPRV1  <- privLevel(if new_sst.SPS then Supervisor else User)
; when chk.SPIE do
   mt.MIE1  <- new_sst.SPIE

-- conditional back-projections
; when chk.SIE and privilege(mst.MPRV) == Supervisor
  do mt.MIE <- new_sst.SIE

; (st, mt)
}

---------------------------------------------------------------------------
-- Register state space
---------------------------------------------------------------------------

-- Each register state is local to a core.

type RegFile    = reg  -> regType

declare
{ c_gpr         :: id -> RegFile                -- general purpose registers
  c_PC          :: id -> regType                -- program counter

  c_UCSR        :: id -> UserCSR                -- user-level CSRs
  c_SCSR        :: id -> SupervisorCSR          -- supervisor-level CSRs
  c_HCSR        :: id -> HypervisorCSR          -- hypervisor-level CSRs
  c_MCSR        :: id -> MachineCSR             -- machine-level CSRs

  -- interpreter execution context
  c_BranchTo    :: id -> regType option         -- requested branch
  c_Exception   :: id -> ExceptionType option   -- exception
  c_ReserveLoad :: id -> regType option         -- load reservation for LL/SC
}

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
{ value        = { m = c_gpr(procID); m(n) }
  assign value = { var m = c_gpr(procID)
                 ; m(n) <- value
                 ; c_gpr(procID) <- m
                 }
}

component PC :: regType
{ value        = c_PC(procID)
  assign value = c_PC(procID) <- value
}

component UCSR :: UserCSR
{  value        = c_UCSR(procID)
   assign value = c_UCSR(procID) <- value
}

component SCSR :: SupervisorCSR
{ value        = c_SCSR(procID)
  assign value = c_SCSR(procID) <- value
}

component HCSR :: HypervisorCSR
{ value        = c_HCSR(procID)
  assign value = c_HCSR(procID) <- value
}

component MCSR :: MachineCSR
{ value        = c_MCSR(procID)
  assign value = c_MCSR(procID) <- value
}

component BranchTo :: regType option
{ value        = c_BranchTo(procID)
  assign value = c_BranchTo(procID) <- value
}

component Exception :: ExceptionType option
{ value        = c_Exception(procID)
  assign value = c_Exception(procID) <- value
}

component ReserveLoad :: regType option
{ value        = c_ReserveLoad(procID)
  assign value = c_ReserveLoad(procID) <- value
}

-- machine state utilities

Architecture curArch() =
    architecture(MCSR.mcpuid.ArchBase)

bool in32BitMode() =
    curArch() == RV32I

unit setArch(a::Architecture) =
    MCSR.mcpuid.ArchBase <- archBase(a)

Privilege curPrivilege() =
    privilege(MCSR.mstatus.MPRV)

---------------------------------------------------------------------------
-- CSR Register address map
---------------------------------------------------------------------------

-- CSR access control

type csrRW    = bits(2)         -- read/write check
type csrPR    = bits(2)         -- privilege check

csrRW csrRW(csr::creg)  = csr<11:10>
csrPR csrPR(csr::creg)  = csr<9:8>

-- this only checks register-level access.  some registers have
-- additional bit-specific read/write controls.
bool check_CSR_access(rw::csrRW, pr::csrPR, p::Privilege, a::accessType) =
    (privLevel(p) >= pr) and (a == Read or rw == 0b11)

-- XXX: Revise this to handle absence of counter regs in RV32E.
bool is_CSR_defined(csr::creg) =
    -- user-mode
 {- XXX: skip since we don't have floating-point yet
    (csr >= 0x001 and csr <= 0x003)
  -}
    (csr >= 0xC00 and csr <= 0xC02)
 or (csr >= 0xC80 and csr <= 0xC82 and in32BitMode())

    -- supervisor-mode
 or (csr >= 0x100 and csr <= 0x101)
 or  csr == 0x104 or  csr == 0x121

 or  csr == 0xD01 or (csr == 0xD81 and in32BitMode())

 or (csr >= 0x140 and csr <= 0x141) or csr == 0x144
 or (csr >= 0xD42 and csr <= 0xD43)

 or (csr >= 0x180 and csr <= 0x181)

 or (csr >= 0x900 and csr <= 0x902)
 or (csr >= 0x980 and csr <= 0x982 and in32BitMode())

    -- machine-mode
 or (csr >= 0xF00 and csr <= 0xF01) or csr == 0xF10
 or (csr >= 0x300 and csr <= 0x302) or csr == 0x304 or csr == 0x321
 or  csr == 0x701 or (csr == 0x741 and in32BitMode())
 or (csr >= 0x340 and csr <= 0x344)
 or (csr >= 0x380 and csr <= 0x385)
 or  csr >= 0xB01 or (csr == 0xB81 and in32BitMode())
 or (csr >= 0x780 and csr <= 0x781)

component CSRMap(csr::creg) :: regType
{
  value =
      match csr
      { -- user counter/timers
        case 0xC00  => c_UCSR(procID).cycle
        case 0xC01  => c_UCSR(procID).time
        case 0xC02  => c_UCSR(procID).instret
        case 0xC80  => SignExtend(c_UCSR(procID).cycle<63:32>)
        case 0xC81  => SignExtend(c_UCSR(procID).time<63:32>)
        case 0xC82  => SignExtend(c_UCSR(procID).instret<63:32>)

        -- supervisor trap setup
        case 0x100  => &lift_mstatus_sstatus(c_MCSR(procID).mstatus,
                                             c_SCSR(procID).sstatus)
        case 0x101  => c_SCSR(procID).stvec
        case 0x104  => &lift_mie_sie(c_MCSR(procID).mie)
        case 0x121  => c_SCSR(procID).stimecmp

        -- supervisor timer
        case 0xD01  => c_SCSR(procID).stime
        case 0xD81  => SignExtend(c_SCSR(procID).stime<63:32>)

        -- supervisor trap handling
        case 0x140  => c_SCSR(procID).sscratch
        case 0x141  => c_SCSR(procID).sepc
        case 0xD42  => c_SCSR(procID).&scause
        case 0xD43  => c_SCSR(procID).sbadaddr
        case 0x144  => &lift_mip_sip(c_MCSR(procID).mip)

        -- supervisor protection and translation
        case 0x180  => c_SCSR(procID).sptbr
        case 0x181  => c_SCSR(procID).sasid

        -- supervisor read/write shadow of user read-only registers
        case 0x900  => c_UCSR(procID).cycle
        case 0x901  => c_UCSR(procID).time
        case 0x902  => c_UCSR(procID).instret
        case 0x980  => SignExtend(c_UCSR(procID).cycle<63:32>)
        case 0x981  => SignExtend(c_UCSR(procID).time<63:32>)
        case 0x982  => SignExtend(c_UCSR(procID).instret<63:32>)

        -- hypervisor trap setup
        case 0x200  => c_HCSR(procID).&hstatus
        case 0x201  => c_HCSR(procID).htvec
        case 0x202  => c_HCSR(procID).&htdeleg
        case 0x221  => c_HCSR(procID).htimecmp

        -- hypervisor timer
        case 0xE01  => c_HCSR(procID).htime
        case 0xE81  => SignExtend(c_HCSR(procID).htime<63:32>)

        -- hypervisor trap handling
        case 0x240  => c_HCSR(procID).hscratch
        case 0x241  => c_HCSR(procID).hepc
        case 0x242  => c_HCSR(procID).&hcause
        case 0x243  => c_HCSR(procID).hbadaddr

        -- hypervisor read/write shadow of supervisor read-only registers
        case 0xA01  => c_SCSR(procID).stime
        case 0xA81  => SignExtend(c_SCSR(procID).stime<63:32>)

        -- machine information registers
        case 0xF00  => c_MCSR(procID).&mcpuid
        case 0xF01  => c_MCSR(procID).&mimpid
        case 0xF10  => c_MCSR(procID).mhartid

        -- machine trap setup
        case 0x300  => c_MCSR(procID).&mstatus
        case 0x301  => c_MCSR(procID).mtvec
        case 0x302  => c_MCSR(procID).&mtdeleg
        case 0x304  => c_MCSR(procID).&mie
        case 0x321  => c_MCSR(procID).mtimecmp

        -- machine timers and counters
        case 0x701  => c_MCSR(procID).mtime
        case 0x741  => SignExtend(c_MCSR(procID).mtime<63:32>)

        -- machine trap handling
        case 0x340  => c_MCSR(procID).mscratch
        case 0x341  => c_MCSR(procID).mepc
        case 0x342  => c_MCSR(procID).&mcause
        case 0x343  => c_MCSR(procID).mbadaddr
        case 0x344  => c_MCSR(procID).&mip

        -- machine protection and translation
        case 0x380  => c_MCSR(procID).mbase
        case 0x381  => c_MCSR(procID).mbound
        case 0x382  => c_MCSR(procID).mibase
        case 0x383  => c_MCSR(procID).mibound
        case 0x384  => c_MCSR(procID).mdbase
        case 0x385  => c_MCSR(procID).mdbound

        -- machine read-write shadow of hypervisor read-only registers
        case 0xB01  => c_HCSR(procID).htime
        case 0xB81  => SignExtend(c_HCSR(procID).htime<63:32>)

        -- machine host-target interface (berkeley extension)
        case 0x780  => c_MCSR(procID).mtohost
        case 0x781  => c_MCSR(procID).mfromhost

        case _      => UNKNOWN -- TODO: should trap as illegal instr
      }

  assign value =
      match csr
      { -- user counters/timers are URO

        -- supervisor trap setup
        case 0x100  =>
        { st_mt = lower_sstatus_mstatus(sstatus(value), c_SCSR(procID).sstatus,
                                        c_MCSR(procID).mstatus)
        ; c_SCSR(procID).sstatus <- Fst(st_mt)
        ; c_MCSR(procID).mstatus <- Snd(st_mt)
        }
        case 0x101  => c_SCSR(procID).stvec             <- value
        -- sie back-projects to mie
        case 0x104  => c_MCSR(procID).mie               <- lower_sie_mie(sie(value), c_MCSR(procID).mie)
        case 0x121  =>
        { c_SCSR(procID).stimecmp <- value
        ; c_MCSR(procID).mip.STIP <- false
        }

        -- supervisor trap handling
        case 0x140  => c_SCSR(procID).sscratch          <- value
        case 0x141  => c_SCSR(procID).sepc              <- (value && SignExtend(0b100`3))  -- no 16-bit instr support
        -- scause, sbadaddr are SRO
        -- sip back-projects to mip
        case 0x144  => c_MCSR(procID).mip               <- lower_sip_mip(sip(value), c_MCSR(procID).mip)

        -- supervisor protection and translation
        case 0x180  => c_SCSR(procID).sptbr             <- value
        case 0x181  => c_SCSR(procID).sasid             <- value

        -- supervisor read/write shadow of user read-only registers
        case 0x900  => c_UCSR(procID).cycle             <- value
        case 0x901  => c_UCSR(procID).time              <- value
        case 0x902  => c_UCSR(procID).instret           <- value
        case 0x980  => c_UCSR(procID).cycle<63:32>      <- value<31:0>
        case 0x981  => c_UCSR(procID).time<63:32>       <- value<31:0>
        case 0x982  => c_UCSR(procID).instret<63:32>    <- value<31:0>


        -- TODO: hypervisor register write support

        -- machine information registers are MRO

        -- machine trap setup
        case 0x300  => c_MCSR(procID).mstatus           <- mstatus(value)
        case 0x301  => c_MCSR(procID).mtvec             <- value
        case 0x302  => c_MCSR(procID).mtdeleg           <- mtdeleg(value)
        case 0x304  => c_MCSR(procID).mie               <- mie(value)
        case 0x321  =>
        { c_MCSR(procID).mtimecmp <- value
        ; c_MCSR(procID).mip.MTIP <- false
        }

        -- machine timers and counters
        case 0x701  => c_MCSR(procID).mtime             <- value
        case 0x741  => c_MCSR(procID).mtime<63:32>      <- value<31:0>

        -- machine trap handling
        case 0x340  => c_MCSR(procID).mscratch          <- value
        case 0x341  => c_MCSR(procID).mepc              <- (value && SignExtend(0b100`3))  -- no 16-bit instr support
        case 0x342  => c_MCSR(procID).mcause            <- mcause(value)
        case 0x343  => c_MCSR(procID).mbadaddr          <- value
        case 0x344  => c_MCSR(procID).mip               <- mip(value)

        -- machine protection and translation
        case 0x380  => c_MCSR(procID).mbase             <- value
        case 0x381  => c_MCSR(procID).mbound            <- value
        case 0x382  => c_MCSR(procID).mibase            <- value
        case 0x383  => c_MCSR(procID).mibound           <- value
        case 0x384  => c_MCSR(procID).mdbase            <- value
        case 0x385  => c_MCSR(procID).mdbound           <- value

        -- machine read-write shadow of hypervisor read-only registers
        case 0xB01  => c_HCSR(procID).htime             <- value
        case 0xB81  => c_HCSR(procID).htime<63:32>      <- value<31:0>

        -- machine host-target interface (berkeley extension)
        -- TODO: XXX: set I/O pending bit
        case 0x780  =>
        { c_MCSR(procID).mtohost    <- value }
        case 0x781  =>
        { c_MCSR(procID).mfromhost  <- value }

        case _      => nothing -- TODO: should never get here (internal error)
      }
}

string csrName(csr::creg) =
    match csr
    { -- user counter/timers
      case 0xC00  => "cycle"
      case 0xC01  => "time"
      case 0xC02  => "instret"
      case 0xC80  => "cycleh"
      case 0xC81  => "timeh"
      case 0xC82  => "instreth"

      -- supervisor trap setup
      case 0x100  => "sstatus"
      case 0x101  => "stvec"
      case 0x104  => "sie"
      case 0x121  => "stimecmp"

      -- supervisor timer
      case 0xD01  => "stime"
      case 0xD81  => "stimeh"

      -- supervisor trap handling
      case 0x140  => "sscratch"
      case 0x141  => "sepc"
      case 0xD42  => "scause"
      case 0xD43  => "sbadaddr"
      case 0x144  => "mip"

      -- supervisor protection and translation
      case 0x180  => "sptbr"
      case 0x181  => "sasid"

      -- supervisor read/write shadow of user read-only registers
      case 0x900  => "cycle"
      case 0x901  => "time"
      case 0x902  => "instret"
      case 0x980  => "cycleh"
      case 0x981  => "timeh"
      case 0x982  => "instreth"

      -- hypervisor trap setup
      case 0x200  => "hstatus"
      case 0x201  => "htvec"
      case 0x202  => "htdeleg"
      case 0x221  => "htimecmp"

      -- hypervisor timer
      case 0xE01  => "htime"
      case 0xE81  => "htimeh"

      -- hypervisor trap handling
      case 0x240  => "hscratch"
      case 0x241  => "hepc"
      case 0x242  => "hcause"
      case 0x243  => "hbadaddr"

      -- hypervisor read/write shadow of supervisor read-only registers
      case 0xA01  => "stime"
      case 0xA81  => "stimeh"

      -- machine information registers
      case 0xF00  => "mcpuid"
      case 0xF01  => "mimpid"
      case 0xF10  => "mhartid"

      -- machine trap setup
      case 0x300  => "mstatus"
      case 0x301  => "mtvec"
      case 0x302  => "mtdeleg"
      case 0x304  => "mie"
      case 0x321  => "mtimecmp"

      -- machine timers and counters
      case 0x701  => "mtime"
      case 0x741  => "mtimeh"

      -- machine trap handling
      case 0x340  => "mscratch"
      case 0x341  => "mepc"
      case 0x342  => "mcause"
      case 0x343  => "mbadaddr"
      case 0x344  => "mip"

      -- machine protection and translation
      case 0x380  => "mbase"
      case 0x381  => "mbound"
      case 0x382  => "mibase"
      case 0x383  => "mibound"
      case 0x384  => "mdbase"
      case 0x385  => "mdbound"

      -- machine read-write shadow of hypervisor read-only registers
      case 0xB01  => "htime"
      case 0xB81  => "htimeh"

      -- machine host-target interface (berkeley extension)
      case 0x780  => "mtohost"
      case 0x781  => "mfromhost"

      case _      => "UNKNOWN"
    }
---------------------------------------------------------------------------
-- Tandem verification
---------------------------------------------------------------------------
-- This describes the state update due to every retired instruction,
-- which can be verified against an external oracle.  Currently, the
-- Cissr tool from Bluespec fills the role, and the record below is
-- designed against its API.

record StateDelta
{ exc_taken     :: bool         -- whether an exception (interrupt/trap) was taken
  pc            :: regType      -- PC of retired instruction

  addr          :: regType      -- address argument for instruction:
                                --   new control flow target for jump, exception branch, ERET
                                --   memory address for memory ops and AMOs
                                --   CSR register address for CSR instructions

  data1         :: regType      -- data result for instruction:
                                --   new value for rd for ALU ops, LOAD, LOAD_FP, LR, SC, CSR ops
                                --   new csr_status for exceptions and ERET

  data2         :: regType      -- data argument for instruction:
                                --   new csr_cause for exceptions
                                --   new memory value for STORE, STORE_FP, SC, AMOs
                                --   argument for CSR ops

  data3         :: regType      -- unused

  fp_data       :: fpval        -- floating point value

  pc_instr      :: word         -- the retired instruction
}

declare c_update :: id -> StateDelta

component Delta :: StateDelta
{  value         = c_update(procID)
   assign value  = c_update(procID) <- value
}

unit setupDelta(pc::regType, instr_word::word) =
{ Delta.exc_taken <- false
; Delta.pc        <- pc
; Delta.addr      <- 0
; Delta.data1     <- 0
; Delta.data2     <- 0
; Delta.data3     <- 0
; Delta.fp_data   <- 0
; Delta.pc_instr  <- instr_word
}

unit recordLoad(addr::vAddr, val::regType) =
{ Delta.addr      <- addr
; Delta.data1     <- val
}

---------------------------------------------------------------------------
-- Logging
---------------------------------------------------------------------------

string log_w_csr(csr::creg, data::regType) =
    "CSR (0x" : PadLeft(#"0", 3, [csr]) : ") <- 0x" : PadLeft(#"0", 16, [data])

string reg(r::reg) =
{ if      r ==  0 then "$0"
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

string log_w_mem_mask(pAddr::pAddr, vAddr::vAddr, mask::regType, data::regType,
                      old::regType, new::regType) =
    "MEM[0x" : PadLeft(#"0", 10, [pAddr]) :
    "/" : PadLeft(#"0", 10, [vAddr]) :
    "] <- (data: 0x" : PadLeft(#"0", 16, [data]) :
    ", mask: 0x" : PadLeft(#"0", 16, [mask]) :
    ", old: 0x"  : PadLeft(#"0", 16, [old]) :
    ", new: 0x"  : PadLeft(#"0", 16, [new]) :
    ")"

string log_w_mem_mask_misaligned(pAddr::pAddr, vAddr::vAddr, mask::regType, data::regType,
                                 align::nat, old::regType, new::regType) =
    "MEM[0x" : PadLeft(#"0", 10, [pAddr]) :
    "/" : PadLeft(#"0", 10, [vAddr]) :
    "/ misaligned@" : [align] :
    "] <- (data: 0x" : PadLeft(#"0", 16, [data]) :
    ", mask: 0x" : PadLeft(#"0", 16, [mask]) :
    ", old: 0x"  : PadLeft(#"0", 16, [old]) :
    ", new: 0x"  : PadLeft(#"0", 16, [new]) :
    ")"

string log_w_mem(pAddr::pAddr, vAddr::vAddr, data::regType) =
    "MEM[0x" : PadLeft(#"0", 10, [pAddr]) :
    "/" : PadLeft(#"0", 10, [vAddr]) :
    "] <- (data: 0x" : PadLeft(#"0", 16, [data]) : ")"

string log_r_mem(pAddr::pAddr, vAddr::vAddr, data::regType) =
    "data <- MEM[0x" : PadLeft(#"0", 10, [pAddr]) :
    "/" : PadLeft(#"0", 10, [vAddr]) :
    "]: 0x" : PadLeft(#"0", 16, [data])

string log_tohost(tohost::regType) =
    "-> host: " : [[tohost<7:0>]::char]

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
    nothing
{- SCSR.cause.Int    <- false
; SCSR.cause.EC     <- excCode(e)
; SCSR.epc          <- PC
; SCSR.status.PS    <- SCSR.status.S
; SCSR.status.S     <- true
; SCSR.status.PEI   <- SCSR.status.EI
; SCSR.status.EI    <- false
; Exception         <- Some(e)
-}

unit signalAddressException(e::ExceptionType, vAddr::vAddr) =
{ MCSR.mbadaddr     <- vAddr
; setupException(e)
}

unit signalException(e::ExceptionType) =
{ MCSR.mbadaddr     <- 0
; setupException(e)
}

unit signalEnvCall() =
{ e = match privilege(MCSR.mstatus.MPRV)
      { case User       => UMode_Env_Call
        case Supervisor => SMode_Env_Call
        case Hypervisor => HMode_Env_Call
        case Machine    => MMode_Env_Call
      }
; setupException(e)
}

---------------------------------------------------------------------------
-- CSR access with logging
---------------------------------------------------------------------------

component CSR(n::creg) :: regType
{ value = CSRMap(n)
  assign value =  { CSRMap(n) <- value
                  ; mark_log(2, log_w_csr(n, value))
                  }
}

unit writeCSR(csr::creg, val::regType) =
{ CSR(csr)      <- val;
  Delta.addr    <- ZeroExtend(csr);
  Delta.data2   <- CSR(csr) -- Note that writes to CSR are intercepted
                            -- and controlled by CSRMap, so we need to
                            -- use what was finally written to the
                            -- CSR, and not val itself.
}

---------------------------------------------------------------------------
-- GPR access with logging
---------------------------------------------------------------------------

component GPR(n::reg) :: regType
{ value = if n == 0 then 0 else gpr(n)
  assign value = when n <> 0 do
                   { gpr(n) <- value
                   ; mark_log(2, log_w_gpr(n, value))
                   }
}

unit writeRD(rd::reg, val::regType) =
{ GPR(rd)       <- val
; Delta.data1   <- val
}

unit dumpRegs() =
{ mark_log(0, "======   Registers   ======")
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
{ pAddr = vAddr<63:3>
; align = [vAddr<2:0>]::nat
; if align == 0   -- aligned read
  then { data = VMEM(pAddr)
       ; mark_log(2, log_r_mem(pAddr,   vAddr, data))
       ; data
       }
  -- TODO: optimize this to avoid the second read when possible
  else { dw0   = VMEM(pAddr)
       ; dw1   = VMEM(pAddr+1)
       ; ddw   = (dw1 : dw0) >> (align * 8)
       ; data  = ddw<63:0>
       ; mark_log(2, log_r_mem(pAddr,   vAddr, dw0))
       ; mark_log(2, log_r_mem(pAddr+1, vAddr, dw1))
       ; mark_log(2, log_r_mem(pAddr,   vAddr, data))
       ; data
       }
}

unit writeData(vAddr::vAddr, data::regType, mask::regType, nbytes::nat) =
{ val   = data && mask
; pAddr = vAddr<63:3>
; align = [vAddr<2:0>] :: nat
; old   = VMEM(pAddr)

; mark_log(2, log_r_mem(pAddr, vAddr, old))

-- The cissr verifier expects to see the full register width, as
-- opposed to the masked value for non-full-width stores.  It should
-- accept either, ideally.
; Delta.data2 <- data
; Delta.addr  <- vAddr

; if align == 0     -- aligned write
  then { new = old && ~mask || val
       ; VMEM(pAddr) <- new
       ; mark_log(2, log_w_mem_mask(pAddr, vAddr, mask, data, old, new))
       }
  else { if align + nbytes <= Size(mask) div 8 -- write to single regType-sized block
         then { new = old && ~(mask << (align * 8)) || val << (align * 8)
              ; VMEM(pAddr) <- new
              ; mark_log(2, log_w_mem_mask_misaligned(pAddr, vAddr, mask, data, align, old, new))
              }
         else { mark_log(0, "XXX write of size " : [nbytes] : " with align " : [align] : " and size " : [nbytes])
              -- TODO: handle this case
              }
       }
}

word readInst(vAddr::vAddr) =
{ pAddr = vAddr<63:3>
; data  = VMEM(pAddr)
; mark_log(2, log_r_mem(pAddr, vAddr, data))
; if vAddr<2> then data<63:32> else data<31:0>
}

-- helper used to preload memory contents
unit writeMem(vAddr::vAddr, data::regType) =
{ pAddr = vAddr<63:3>
; VMEM(pAddr) <- data
; mark_log(2, log_w_mem(pAddr, vAddr, data))
}

---------------------------------------------------------------------------
-- Control Flow
---------------------------------------------------------------------------

unit branchTo(newPC::regType) =
{ BranchTo   <- Some(newPC)
; Delta.addr <- newPC
}

unit noBranch(nextPC::regType) =
    Delta.addr <- nextPC

--------------------------------------------------
-- Instruction fetch
--------------------------------------------------

word option Fetch() =
{ pc    = PC
; inst  = readInst(pc)
; setupDelta(pc, inst)
; Some(inst)
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
    if in32BitMode() then
        signalException(Illegal_Instr)
    else { temp = GPR(rs1) + SignExtend(imm)
         ; writeRD(rd, SignExtend(temp<31:0>))
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
    if in32BitMode() and imm<5> then
        signalException(Illegal_Instr)
    else
        writeRD(rd, GPR(rs1) << [imm])

-----------------------------------
-- SRLI  rd, rs1, imm
-----------------------------------
define Shift > SRLI(rd::reg, rs1::reg, imm::bits(6)) =
    if in32BitMode() and imm<5> then
        signalException(Illegal_Instr)
    else
        writeRD(rd, GPR(rs1) >>+ [imm])

-----------------------------------
-- SRAI  rd, rs1, imm
-----------------------------------
define Shift > SRAI(rd::reg, rs1::reg, imm::bits(6)) =
    if in32BitMode() and imm<5> then
        signalException(Illegal_Instr)
    else
        writeRD(rd, GPR(rs1) >> [imm])

------------------------------------------
-- utility function for shift instructions
------------------------------------------
bool notWordValue(value::regType) =
{ top = value<63:32>
; if value<31> then top <> 0xFFFF_FFFF
  else              top <> 0x0
}

-----------------------------------
-- SLLIW rd, rs1, imm   (RV64I)
-----------------------------------
define Shift > SLLIW(rd::reg, rs1::reg, imm::bits(5)) =
    if in32BitMode() or notWordValue(GPR(rs1)) then
        signalException(Illegal_Instr)
    else
        writeRD(rd, SignExtend(GPR(rs1)<31:0> << [imm]))

-----------------------------------
-- SRLIW rd, rs1, imm   (RV64I)
-----------------------------------
define Shift > SRLIW(rd::reg, rs1::reg, imm::bits(5)) =
    if in32BitMode() or notWordValue(GPR(rs1)) then
        signalException(Illegal_Instr)
    else
        writeRD(rd, SignExtend(GPR(rs1)<31:0> >>+ [imm]))

-----------------------------------
-- SRAIW rd, rs1, imm   (RV64I)
-----------------------------------
define Shift > SRAIW(rd::reg, rs1::reg, imm::bits(5)) =
    if in32BitMode() or notWordValue(GPR(rs1)) then
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
    if in32BitMode() then
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
    if in32BitMode() then
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
    if in32BitMode() then
        writeRD(rd, GPR(rs1) << ZeroExtend(GPR(rs2)<4:0>))
    else
        writeRD(rd, GPR(rs1) << ZeroExtend(GPR(rs2)<5:0>))

-----------------------------------
-- SLLW  rd, rs1, rs2   (RV64I)
-----------------------------------
define Shift > SLLW(rd::reg, rs1::reg, rs2::reg) =
    if in32BitMode() then
        signalException(Illegal_Instr)
    else
        writeRD(rd, SignExtend(GPR(rs1)<31:0> << ZeroExtend(GPR(rs2)<4:0>)))

-----------------------------------
-- SRL   rd, rs1, rs2
-----------------------------------
define Shift > SRL(rd::reg, rs1::reg, rs2::reg) =
    if in32BitMode() then
        writeRD(rd, GPR(rs1) >>+ ZeroExtend(GPR(rs2)<4:0>))
    else
        writeRD(rd, GPR(rs1) >>+ ZeroExtend(GPR(rs2)<5:0>))

-----------------------------------
-- SRLW  rd, rs1, rs2   (RV64I)
-----------------------------------
define Shift > SRLW(rd::reg, rs1::reg, rs2::reg) =
    if in32BitMode() then
        signalException(Illegal_Instr)
    else
        writeRD(rd, SignExtend(GPR(rs1)<31:0> >>+ ZeroExtend(GPR(rs2)<4:0>)))

-----------------------------------
-- SRA   rd, rs1, rs2
-----------------------------------
define Shift > SRA(rd::reg, rs1::reg, rs2::reg) =
    if in32BitMode() then
        writeRD(rd, GPR(rs1) >> ZeroExtend(GPR(rs2)<4:0>))
    else
        writeRD(rd, GPR(rs1) >> ZeroExtend(GPR(rs2)<5:0>))

-----------------------------------
-- SRAW  rd, rs1, rs2   (RV64I)
-----------------------------------
define Shift > SRAW(rd::reg, rs1::reg, rs2::reg) =
    if in32BitMode() then
        signalException(Illegal_Instr)
    else
        writeRD(rd, SignExtend(GPR(rs1)<31:0> >> ZeroExtend(GPR(rs2)<4:0>)))

---------------------------------------------------------------------------
-- Multiply and Divide
---------------------------------------------------------------------------

-- Most of the MulDiv implemention assumes we are RV64.

-----------------------------------
-- MUL   rd, rs1, rs2
-----------------------------------
define MulDiv > MUL(rd::reg, rs1::reg, rs2::reg) =
    writeRD(rd, GPR(rs1) * GPR(rs2))

-----------------------------------
-- MULH  rd, rs1, rs2
-----------------------------------
define MulDiv > MULH(rd::reg, rs1::reg, rs2::reg) =
{ prod`128 = SignExtend(GPR(rs1)) * SignExtend(GPR(rs2))
; writeRD(rd, prod<127:64>)
}

-----------------------------------
-- MULHU rd, rs1, rs2
-----------------------------------
define MulDiv > MULHU(rd::reg, rs1::reg, rs2::reg) =
{ prod`128 = ZeroExtend(GPR(rs1)) * ZeroExtend(GPR(rs2))
; writeRD(rd, prod<127:64>)
}

-----------------------------------
-- MULHSU rd, rs1, rs2
-----------------------------------
define MulDiv > MULHSU(rd::reg, rs1::reg, rs2::reg) =
{ prod`128 = SignExtend(GPR(rs1)) * ZeroExtend(GPR(rs2))
; writeRD(rd, prod<127:64>)
}

-----------------------------------
-- MULW  rd, rs1, rs2
-----------------------------------
define MulDiv > MULW(rd::reg, rs1::reg, rs2::reg) =
    if in32BitMode() then
        signalException(Illegal_Instr)
    else { prod`64 = SignExtend(GPR(rs1)<31:0> * GPR(rs2)<31:0>)
         ; writeRD(rd, SignExtend(prod<31:0>))
         }

-----------------------------------
-- DIV   rd, rs1, rs2
-----------------------------------
define MulDiv > DIV(rd::reg, rs1::reg, rs2::reg) =
    if GPR(rs2) == 0x0 then
        writeRD(rd, SignExtend(1`1))
    else { minus_one::regType   = SignExtend(1`1)
         ; minus_max            = 0b1 << (Size(GPR(rs1)) - 1)
         ; if GPR(rs1) == minus_max and GPR(rs2) == minus_one then
               writeRD(rd, minus_max)
           else
               writeRD(rd, GPR(rs1) quot GPR(rs2))
         }

-----------------------------------
-- REM   rd, rs1, rs2
-----------------------------------
define MulDiv > REM(rd::reg, rs1::reg, rs2::reg) =
    if GPR(rs2) == 0x0 then
        writeRD(rd, GPR(rs1))
    else { minus_one::regType   = SignExtend(1`1)
         ; minus_max            = 0b1 << (Size(GPR(rs1)) - 1)
         ; if GPR(rs1) == minus_max and GPR(rs2) == minus_one then
               writeRD(rd, 0)
           else
               writeRD(rd, GPR(rs1) rem GPR(rs2))
         }

-----------------------------------
-- DIVU  rd, rs1, rs2
-----------------------------------
define MulDiv > DIVU(rd::reg, rs1::reg, rs2::reg) =
    if GPR(rs2) == 0x0 then
        writeRD(rd, SignExtend(1`1))
    else
        writeRD(rd, GPR(rs1) div GPR(rs2))

-----------------------------------
-- REMU  rd, rs1, rs2
-----------------------------------
define MulDiv > REMU(rd::reg, rs1::reg, rs2::reg) =
    if GPR(rs2) == 0x0 then
        writeRD(rd, GPR(rs1))
    else
        writeRD(rd, GPR(rs1) mod GPR(rs2))

-----------------------------------
-- DIVW  rd, rs1, rs2
-----------------------------------
define MulDiv > DIVW(rd::reg, rs1::reg, rs2::reg) =
    if in32BitMode() then
        signalException(Illegal_Instr)
    else { s1 = GPR(rs1)<31:0>
         ; s2 = GPR(rs2)<31:0>
         ; if s2 == 0x0 then
               writeRD(rd, SignExtend(1`1))
           else { minus_one::word    = SignExtend(1`1)
                ; minus_max          = 0b1 << (Size(s1) - 1)
                ; if s1 == minus_max and s2 == minus_one then
                      writeRD(rd, SignExtend(minus_max))
                  else
                      writeRD(rd, SignExtend(s1 quot s2))
                }
         }

-----------------------------------
-- REMW  rd, rs1, rs2
-----------------------------------
define MulDiv > REMW(rd::reg, rs1::reg, rs2::reg) =
    if in32BitMode() then
        signalException(Illegal_Instr)
    else { s1 = GPR(rs1)<31:0>
         ; s2 = GPR(rs2)<31:0>
         ; if s2 == 0x0 then
               writeRD(rd, SignExtend(s1))
           else
               writeRD(rd, SignExtend(s1 rem s2))
         }

-----------------------------------
-- DIVUW rd, rs1, rs2
-----------------------------------
define MulDiv > DIVUW(rd::reg, rs1::reg, rs2::reg) =
    if in32BitMode() then
        signalException(Illegal_Instr)
    else { s1 = GPR(rs1)<31:0>
         ; s2 = GPR(rs2)<31:0>
         ; if s2 == 0x0 then
               writeRD(rd, SignExtend(1`1))
           else
               writeRD(rd, SignExtend(s1 div s2))
         }

-----------------------------------
-- REMUW rd, rs1, rs2
-----------------------------------
define MulDiv > REMUW(rd::reg, rs1::reg, rs2::reg) =
    if in32BitMode() then
        signalException(Illegal_Instr)
    else { s1 = GPR(rs1)<31:0>
         ; s2 = GPR(rs2)<31:0>
         ; if s2 == 0x0 then
               writeRD(rd, SignExtend(s1))
           else
               writeRD(rd, SignExtend(s1 mod s2))
         }

---------------------------------------------------------------------------
-- Control Transfer Instructions
---------------------------------------------------------------------------

-- Unconditional jumps

-----------------------------------
-- JAL   rd, offs
-----------------------------------
define Branch > JAL(rd::reg, imm::imm20) =
{ writeRD(rd, PC + 4)
; branchTo(PC + SignExtend(imm << 1))
}

-----------------------------------
-- JALR  rd, rs1, imm
-----------------------------------
define Branch > JALR(rd::reg, rs1::reg, imm::imm12) =
{ temp      = GPR(rs1) + SignExtend(imm)
; writeRD(rd, PC + 4)
; branchTo(temp && SignExtend('10'))
}

-- conditional branches

-----------------------------------
-- BEQ   rs1, rs2, offs
-----------------------------------
define Branch > BEQ(rs1::reg, rs2::reg, offs::imm12) =
    if GPR(rs1) == GPR(rs2) then
        branchTo(PC + (SignExtend(offs) << 1))
    else
        noBranch(PC + 4)

-----------------------------------
-- BNE   rs1, rs2, offs
-----------------------------------
define Branch > BNE(rs1::reg, rs2::reg, offs::imm12) =
    if GPR(rs1) <> GPR(rs2) then
        branchTo(PC + (SignExtend(offs) << 1))
    else
        noBranch(PC + 4)

-----------------------------------
-- BLT   rs1, rs2, offs
-----------------------------------
define Branch > BLT(rs1::reg, rs2::reg, offs::imm12) =
    if GPR(rs1) < GPR(rs2) then
        branchTo(PC + (SignExtend(offs) << 1))
    else
        noBranch(PC + 4)

-----------------------------------
-- BLTU  rs1, rs2, offs
-----------------------------------
define Branch > BLTU(rs1::reg, rs2::reg, offs::imm12) =
    if GPR(rs1) <+ GPR(rs2) then
        branchTo(PC + (SignExtend(offs) << 1))
    else
        noBranch(PC + 4)

-----------------------------------
-- BGE   rs1, rs2, offs
-----------------------------------
define Branch > BGE(rs1::reg, rs2::reg, offs::imm12) =
    if GPR(rs1) >= GPR(rs2) then
        branchTo(PC + (SignExtend(offs) << 1))
    else
        noBranch(PC + 4)

-----------------------------------
-- BGEU  rs1, rs2, offs
-----------------------------------
define Branch > BGEU(rs1::reg, rs2::reg, offs::imm12) =
    if GPR(rs1) >=+ GPR(rs2) then
        branchTo(PC + (SignExtend(offs) << 1))
    else
        noBranch(PC + 4)

---------------------------------------------------------------------------
-- Load and Store Instructions
---------------------------------------------------------------------------

-----------------------------------
-- LW    rd, rs1, offs
-----------------------------------
define Load > LW(rd::reg, rs1::reg, offs::imm12) =
{ addr = GPR(rs1) + SignExtend(offs)
; val  = SignExtend(readData(addr)<31:0>)
; GPR(rd) <- val
; recordLoad(addr, val)
}

-----------------------------------
-- LWU   rd, rs1, offs  (RV64I)
-----------------------------------
define Load > LWU(rd::reg, rs1::reg, offs::imm12) =
{ if in32BitMode() then
      signalException(Illegal_Instr)
  else { addr = GPR(rs1) + SignExtend(offs)
       ; val  = ZeroExtend(readData(addr)<31:0>)
       ; GPR(rd) <- val
       ; recordLoad(addr, val)
       }
}

-----------------------------------
-- LH    rd, rs1, offs
-----------------------------------
define Load > LH(rd::reg, rs1::reg, offs::imm12) =
{ addr = GPR(rs1) + SignExtend(offs)
; val  = SignExtend(readData(addr)<15:0>)
; GPR(rd) <- val
; recordLoad(addr, val)
}

-----------------------------------
-- LHU   rd, rs1, offs
-----------------------------------
define Load > LHU(rd::reg, rs1::reg, offs::imm12) =
{ addr = GPR(rs1) + SignExtend(offs)
; val  = ZeroExtend(readData(addr)<15:0>)
; GPR(rd) <- val
; recordLoad(addr, val)
}

-----------------------------------
-- LB    rd, rs1, offs
-----------------------------------
define Load > LB(rd::reg, rs1::reg, offs::imm12) =
{ addr = GPR(rs1) + SignExtend(offs)
; val  = SignExtend(readData(addr)<7:0>)
; GPR(rd) <- val
; recordLoad(addr, val)
}

-----------------------------------
-- LBU   rd, rs1, offs
-----------------------------------
define Load > LBU(rd::reg, rs1::reg, offs::imm12) =
{ addr = GPR(rs1) + SignExtend(offs)
; val  = ZeroExtend(readData(addr)<7:0>)
; GPR(rd) <- val
; recordLoad(addr, val)
}

-----------------------------------
-- LD    rd, rs1, offs  (RV64I)
-----------------------------------
define Load > LD(rd::reg, rs1::reg, offs::imm12) =
    if in32BitMode() then
        signalException(Illegal_Instr)
    else { addr = GPR(rs1) + SignExtend(offs)
         ; val  = readData(addr)
         ; GPR(rd) <- val
         ; recordLoad(addr, val)
         }

-----------------------------------
-- SW    rs1, rs2, offs
-----------------------------------
define Store > SW(rs1::reg, rs2::reg, offs::imm12) =
{ addr = GPR(rs1) + SignExtend(offs)
; mask = 0xFFFF_FFFF
; data = GPR(rs2)
; writeData(addr, data, mask, 4)
}

-----------------------------------
-- SH    rs1, rs2, offs
-----------------------------------
define Store > SH(rs1::reg, rs2::reg, offs::imm12) =
{ addr = GPR(rs1) + SignExtend(offs)
; mask = 0xFFFF
; data = GPR(rs2)
; writeData(addr, data, mask, 2)
}

-----------------------------------
-- SB    rs1, rs2, offs
-----------------------------------
define Store > SB(rs1::reg, rs2::reg, offs::imm12) =
{ addr = GPR(rs1) + SignExtend(offs)
; mask = 0xFF
; data = GPR(rs2)
; writeData(addr, data, mask, 1)
}

-----------------------------------
-- SD    rs1, rs2, offs (RV64I)
-----------------------------------
define Store > SD(rs1::reg, rs2::reg, offs::imm12) =
    if in32BitMode() then
        signalException(Illegal_Instr)
    else { addr = GPR(rs1) + SignExtend(offs)
         ; data = GPR(rs2)
         ; writeData(addr, data, SignExtend('1'), 8)
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

-- Atomics --

-----------------------------------
-- LR.W [aq,rl] rd, rs1
-----------------------------------

define AMO > LR_W(aq::amo, rl::amo, rd::reg, rs1::reg) =
    nothing -- TODO

-----------------------------------
-- LR.D [aq,rl] rd, rs1
-----------------------------------

define AMO > LR_D(aq::amo, rl::amo, rd::reg, rs1::reg) =
    nothing -- TODO

-----------------------------------
-- SC.W [aq,rl] rd, rs1, rs2
-----------------------------------

define AMO > SC_W(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
    nothing -- TODO

-----------------------------------
-- SC.D [aq,rl] rd, rs1, rs2
-----------------------------------

define AMO > SC_D(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
    nothing -- TODO

-----------------------------------
-- AMOSWAP.W [aq,rl] rd, rs1, rs2
-----------------------------------

define AMO > AMOSWAP_W(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
{ addr = GPR(rs1)
; if addr<1:0> != 0 then
      signalException(Store_Misaligned)             -- XXX: fix exc name
  else { memv = SignExtend(readData(addr)<31:0>)
       ; data = GPR(rs2)
       ; GPR(rd) <- memv
       ; mask = 0xFFFF_FFFF
       ; writeData(addr, data, mask, 4)
       ; recordLoad(addr, memv)
       }
}

-----------------------------------
-- AMOSWAP.D [aq,rl] rd, rs1, rs2
-----------------------------------

define AMO > AMOSWAP_D(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
{ addr = GPR(rs1)
; if addr<2:0> != 0 then
      signalException(Store_Misaligned)             -- XXX: fix exc name
  else { memv = readData(addr)
       ; data = GPR(rs2)
       ; GPR(rd) <- memv
       ; writeData(addr, data, SignExtend('1'), 8)
       ; recordLoad(addr, memv)
       }
}

-----------------------------------
-- AMOADD.W [aq,rl] rd, rs1, rs2
-----------------------------------

define AMO > AMOADD_W(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
{ addr = GPR(rs1)
; if addr<1:0> != 0 then
      signalException(Store_Misaligned)             -- XXX: fix exc name
  else { memv = SignExtend(readData(addr)<31:0>)
       ; data = GPR(rs2)
       ; GPR(rd) <- memv
       ; val  = data + memv
       ; mask = 0xFFFF_FFFF
       ; writeData(addr, val, mask, 4)
       ; recordLoad(addr, memv)
       }
}

-----------------------------------
-- AMOADD.D [aq,rl] rd, rs1, rs2
-----------------------------------

define AMO > AMOADD_D(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
{ addr = GPR(rs1)
; if addr<2:0> != 0 then
      signalException(Store_Misaligned)             -- XXX: fix exc name
  else { memv = readData(addr)
       ; data = GPR(rs2)
       ; GPR(rd) <- memv
       ; val  = data + memv
       ; writeData(addr, val, SignExtend('1'), 8)
       ; recordLoad(addr, memv)
       }
}

-----------------------------------
-- AMOXOR.W [aq,rl] rd, rs1, rs2
-----------------------------------

define AMO > AMOXOR_W(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
{ addr = GPR(rs1)
; if addr<1:0> != 0 then
      signalException(Store_Misaligned)             -- XXX: fix exc name
  else { memv = SignExtend(readData(addr)<31:0>)
       ; data = GPR(rs2)
       ; GPR(rd) <- memv
       ; val  = data ?? memv
       ; mask = 0xFFFF_FFFF
       ; writeData(addr, val, mask, 4)
       ; recordLoad(addr, memv)
       }
}

-----------------------------------
-- AMOXOR.D [aq,rl] rd, rs1, rs2
-----------------------------------

define AMO > AMOXOR_D(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
{ addr = GPR(rs1)
; if addr<2:0> != 0 then
      signalException(Store_Misaligned)             -- XXX: fix exc name
  else { memv = readData(addr)
       ; data = GPR(rs2)
       ; GPR(rd) <- memv
       ; val  = data ?? memv
       ; writeData(addr, val, SignExtend('1'), 8)
       ; recordLoad(addr, memv)
       }
}

-----------------------------------
-- AMOAND.W [aq,rl] rd, rs1, rs2
-----------------------------------

define AMO > AMOAND_W(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
    { addr = GPR(rs1)
; if addr<1:0> != 0 then
      signalException(Store_Misaligned)             -- XXX: fix exc name
  else { memv = SignExtend(readData(addr)<31:0>)
       ; data = GPR(rs2)
       ; GPR(rd) <- memv
       ; val  = data && memv
       ; mask = 0xFFFF_FFFF
       ; writeData(addr, val, mask, 4)
       ; recordLoad(addr, memv)
       }
}

-----------------------------------
-- AMOAND.D [aq,rl] rd, rs1, rs2
-----------------------------------

define AMO > AMOAND_D(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
{ addr = GPR(rs1)
; if addr<2:0> != 0 then
      signalException(Store_Misaligned)             -- XXX: fix exc name
  else { memv = readData(addr)
       ; data = GPR(rs2)
       ; GPR(rd) <- memv
       ; val  = data && memv
       ; writeData(addr, val, SignExtend('1'), 8)
       ; recordLoad(addr, memv)
       }
}

-----------------------------------
-- AMOOR.W [aq,rl] rd, rs1, rs2
-----------------------------------

define AMO > AMOOR_W(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
    { addr = GPR(rs1)
; if addr<1:0> != 0 then
      signalException(Store_Misaligned)             -- XXX: fix exc name
  else { memv = SignExtend(readData(addr)<31:0>)
       ; data = GPR(rs2)
       ; GPR(rd) <- memv
       ; val  = data || memv
       ; mask = 0xFFFF_FFFF
       ; writeData(addr, val, mask, 4)
       ; recordLoad(addr, memv)
       }
}

-----------------------------------
-- AMOOR.D [aq,rl] rd, rs1, rs2
-----------------------------------

define AMO > AMOOR_D(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
{ addr = GPR(rs1)
; if addr<2:0> != 0 then
      signalException(Store_Misaligned)             -- XXX: fix exc name
  else { memv = readData(addr)
       ; data = GPR(rs2)
       ; GPR(rd) <- memv
       ; val  = data || memv
       ; writeData(addr, val, SignExtend('1'), 8)
       ; recordLoad(addr, memv)
       }
}

-----------------------------------
-- AMOMIN.W [aq,rl] rd, rs1, rs2
-----------------------------------

define AMO > AMOMIN_W(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
    { addr = GPR(rs1)
; if addr<1:0> != 0 then
      signalException(Store_Misaligned)             -- XXX: fix exc name
  else { memv = SignExtend(readData(addr)<31:0>)
       ; data = GPR(rs2)
       ; GPR(rd) <- memv
       ; val  = SignedMin(data, memv)
       ; mask = 0xFFFF_FFFF
       ; writeData(addr, val, mask, 4)
       ; recordLoad(addr, memv)
       }
}

-----------------------------------
-- AMOMIN.D [aq,rl] rd, rs1, rs2
-----------------------------------

define AMO > AMOMIN_D(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
{ addr = GPR(rs1)
; if addr<2:0> != 0 then
      signalException(Store_Misaligned)             -- XXX: fix exc name
  else { memv = readData(addr)
       ; data = GPR(rs2)
       ; GPR(rd) <- memv
       ; val  = SignedMin(data, memv)
       ; writeData(addr, val, SignExtend('1'), 8)
       ; recordLoad(addr, memv)
       }
}

-----------------------------------
-- AMOMAX.W [aq,rl] rd, rs1, rs2
-----------------------------------

define AMO > AMOMAX_W(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
    { addr = GPR(rs1)
; if addr<1:0> != 0 then
      signalException(Store_Misaligned)             -- XXX: fix exc name
  else { memv = SignExtend(readData(addr)<31:0>)
       ; data = GPR(rs2)
       ; GPR(rd) <- memv
       ; val  = SignedMax(data, memv)
       ; mask = 0xFFFF_FFFF
       ; writeData(addr, val, mask, 4)
       ; recordLoad(addr, memv)
       }
}

-----------------------------------
-- AMOMAX.D [aq,rl] rd, rs1, rs2
-----------------------------------

define AMO > AMOMAX_D(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
{ addr = GPR(rs1)
; if addr<2:0> != 0 then
      signalException(Store_Misaligned)             -- XXX: fix exc name
  else { memv = readData(addr)
       ; data = GPR(rs2)
       ; GPR(rd) <- memv
       ; val  = SignedMax(data, memv)
       ; writeData(addr, val, SignExtend('1'), 8)
       ; recordLoad(addr, memv)
       }
}

-----------------------------------
-- AMOMINU.W [aq,rl] rd, rs1, rs2
-----------------------------------

define AMO > AMOMINU_W(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
    { addr = GPR(rs1)
; if addr<1:0> != 0 then
      signalException(Store_Misaligned)             -- XXX: fix exc name
  else { memv = SignExtend(readData(addr)<31:0>)
       ; data = GPR(rs2)
       ; GPR(rd) <- memv
       ; val  = Min(data, memv)
       ; mask = 0xFFFF_FFFF
       ; writeData(addr, val, mask, 4)
       ; recordLoad(addr, memv)
       }
}

-----------------------------------
-- AMOMINU.D [aq,rl] rd, rs1, rs2
-----------------------------------

define AMO > AMOMINU_D(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
{ addr = GPR(rs1)
; if addr<2:0> != 0 then
      signalException(Store_Misaligned)             -- XXX: fix exc name
  else { memv = readData(addr)
       ; data = GPR(rs2)
       ; GPR(rd) <- memv
       ; val  = Min(data, memv)
       ; writeData(addr, val, SignExtend('1'), 8)
       ; recordLoad(addr, memv)
       }
}

-----------------------------------
-- AMOMAXU.W [aq,rl] rd, rs1, rs2
-----------------------------------

define AMO > AMOMAXU_W(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
    { addr = GPR(rs1)
; if addr<1:0> != 0 then
      signalException(Store_Misaligned)             -- XXX: fix exc name
  else { memv = SignExtend(readData(addr)<31:0>)
       ; data = GPR(rs2)
       ; GPR(rd) <- memv
       ; val  = Max(data, memv)
       ; mask = 0xFFFF_FFFF
       ; writeData(addr, val, mask, 4)
       ; recordLoad(addr, memv)
       }
}

-----------------------------------
-- AMOMAXU.D [aq,rl] rd, rs1, rs2
-----------------------------------

define AMO > AMOMAXU_D(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
{ addr = GPR(rs1)
; if addr<2:0> != 0 then
      signalException(Store_Misaligned)             -- XXX: fix exc name
  else { memv = readData(addr)
       ; data = GPR(rs2)
       ; GPR(rd) <- memv
       ; val  = Max(data, memv)
       ; writeData(addr, val, SignExtend('1'), 8)
       ; recordLoad(addr, memv)
       }
}

---------------------------------------------------------------------------
-- System Instructions
---------------------------------------------------------------------------

-----------------------------------
-- ECALL
-----------------------------------
define System > ECALL  = signalEnvCall()

-----------------------------------
-- EBREAK
-----------------------------------

define System > EBREAK = signalException(Breakpoint)

-----------------------------------
-- ECALL
-----------------------------------
define System > ERET   = nothing

-- Control and Status Registers

bool checkCSROp(csr::imm12, rs1::reg) =
{ op = if rs1 == 0x0 then Read else Write
; (is_CSR_defined(csr))
  and (check_CSR_access(csrRW(csr), csrPR(csr), curPrivilege(), op))
}

-----------------------------------
-- CSRRW  rd, rs1, imm
-----------------------------------

define System > CSRRW(rd::reg, rs1::reg, csr::imm12) =
    if checkCSROp(csr, rs1)
    then { val = CSR(csr)
         ; writeCSR(csr, GPR(rs1))
         ; writeRD(rd, val)
         }
    else signalException(Illegal_Instr)

-- TODO: use a more general write function that can mask unwritable bits
-- TODO: handle special case of no side-effects when GPR(rs1) == 0

-----------------------------------
-- CSRRS  rd, rs1, imm
-----------------------------------
define System > CSRRS(rd::reg, rs1::reg, csr::imm12) =
    if checkCSROp(csr, rs1)
    then { val = CSR(csr)
         ; writeCSR(csr, val || GPR(rs1))
         ; writeRD(rd, val)
         }
    else signalException(Illegal_Instr)

-----------------------------------
-- CSRRC  rd, rs1, imm
-----------------------------------
define System > CSRRC(rd::reg, rs1::reg, csr::imm12) =
    if checkCSROp(csr, rs1)
    then { val = CSR(csr)
         ; writeCSR(csr, val && ~GPR(rs1))
         ; writeRD(rd, val)
         }
    else signalException(Illegal_Instr)

-----------------------------------
-- CSRRWI rd, rs1, imm
-----------------------------------
define System > CSRRWI(rd::reg, zimm::reg, csr::imm12) =
    if checkCSROp(csr, zimm)
    then { val = CSR(csr)
         ; writeCSR(csr, ZeroExtend(zimm))
         ; writeRD(rd, val)
         }
    else signalException(Illegal_Instr)

-----------------------------------
-- CSRRSI rd, rs1, imm
-----------------------------------
define System > CSRRSI(rd::reg, zimm::reg, csr::imm12) =
    if checkCSROp(csr, zimm)
    then { val = CSR(csr)
         ; writeCSR(csr, val || ZeroExtend(zimm))
         ; writeRD(rd, val)
         }
    else signalException(Illegal_Instr)

-----------------------------------
-- CSRRCI rd, rs1, imm
-----------------------------------
define System > CSRRCI(rd::reg, zimm::reg, csr::imm12) =
    if checkCSROp(csr, zimm)
    then { val = CSR(csr)
         ; writeCSR(csr, val && ~ZeroExtend(zimm))
         ; writeRD(rd, val)
         }
    else signalException(Illegal_Instr)

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
   { case 'i12 ihi rs2 rs1 000 ilo i11 11000 11' => Branch( BEQ(rs1, rs2, asImm12(i12, i11, ihi, ilo)))
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

     case '0000001   rs2 rs1 000  rd 01100 11' => MulDiv(   MUL(rd, rs1, rs2))
     case '0000001   rs2 rs1 001  rd 01100 11' => MulDiv(  MULH(rd, rs1, rs2))
     case '0000001   rs2 rs1 010  rd 01100 11' => MulDiv(MULHSU(rd, rs1, rs2))
     case '0000001   rs2 rs1 011  rd 01100 11' => MulDiv( MULHU(rd, rs1, rs2))
     case '0000001   rs2 rs1 100  rd 01100 11' => MulDiv(   DIV(rd, rs1, rs2))
     case '0000001   rs2 rs1 101  rd 01100 11' => MulDiv(  DIVU(rd, rs1, rs2))
     case '0000001   rs2 rs1 110  rd 01100 11' => MulDiv(   REM(rd, rs1, rs2))
     case '0000001   rs2 rs1 111  rd 01100 11' => MulDiv(  REMU(rd, rs1, rs2))

     case '0000001   rs2 rs1 000  rd 01110 11' => MulDiv(  MULW(rd, rs1, rs2))
     case '0000001   rs2 rs1 100  rd 01110 11' => MulDiv(  DIVW(rd, rs1, rs2))
     case '0000001   rs2 rs1 101  rd 01110 11' => MulDiv( DIVUW(rd, rs1, rs2))
     case '0000001   rs2 rs1 110  rd 01110 11' => MulDiv(  REMW(rd, rs1, rs2))
     case '0000001   rs2 rs1 111  rd 01110 11' => MulDiv( REMUW(rd, rs1, rs2))

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

     case '00010 aq rl 00000  rs1 010 rd 01011 11' => AMO(     LR_W(aq, rl, rd, rs1))
     case '00010 aq rl 00000  rs1 011 rd 01011 11' => AMO(     LR_D(aq, rl, rd, rs1))
     case '00011 aq rl rs2    rs1 010 rd 01011 11' => AMO(     SC_W(aq, rl, rd, rs1, rs2))
     case '00011 aq rl rs2    rs1 011 rd 01011 11' => AMO(     SC_D(aq, rl, rd, rs1, rs2))

     case '00001 aq rl rs2    rs1 010 rd 01011 11' => AMO(AMOSWAP_W(aq, rl, rd, rs1, rs2))
     case '00000 aq rl rs2    rs1 010 rd 01011 11' => AMO( AMOADD_W(aq, rl, rd, rs1, rs2))
     case '00100 aq rl rs2    rs1 010 rd 01011 11' => AMO( AMOXOR_W(aq, rl, rd, rs1, rs2))
     case '01100 aq rl rs2    rs1 010 rd 01011 11' => AMO( AMOAND_W(aq, rl, rd, rs1, rs2))
     case '01000 aq rl rs2    rs1 010 rd 01011 11' => AMO(  AMOOR_W(aq, rl, rd, rs1, rs2))
     case '10000 aq rl rs2    rs1 010 rd 01011 11' => AMO( AMOMIN_W(aq, rl, rd, rs1, rs2))
     case '10100 aq rl rs2    rs1 010 rd 01011 11' => AMO( AMOMAX_W(aq, rl, rd, rs1, rs2))
     case '11000 aq rl rs2    rs1 010 rd 01011 11' => AMO(AMOMINU_W(aq, rl, rd, rs1, rs2))
     case '11100 aq rl rs2    rs1 010 rd 01011 11' => AMO(AMOMAXU_W(aq, rl, rd, rs1, rs2))

     case '00001 aq rl rs2    rs1 011 rd 01011 11' => AMO(AMOSWAP_D(aq, rl, rd, rs1, rs2))
     case '00000 aq rl rs2    rs1 011 rd 01011 11' => AMO( AMOADD_D(aq, rl, rd, rs1, rs2))
     case '00100 aq rl rs2    rs1 011 rd 01011 11' => AMO( AMOXOR_D(aq, rl, rd, rs1, rs2))
     case '01100 aq rl rs2    rs1 011 rd 01011 11' => AMO( AMOAND_D(aq, rl, rd, rs1, rs2))
     case '01000 aq rl rs2    rs1 011 rd 01011 11' => AMO(  AMOOR_D(aq, rl, rd, rs1, rs2))
     case '10000 aq rl rs2    rs1 011 rd 01011 11' => AMO( AMOMIN_D(aq, rl, rd, rs1, rs2))
     case '10100 aq rl rs2    rs1 011 rd 01011 11' => AMO( AMOMAX_D(aq, rl, rd, rs1, rs2))
     case '11000 aq rl rs2    rs1 011 rd 01011 11' => AMO(AMOMINU_D(aq, rl, rd, rs1, rs2))
     case '11100 aq rl rs2    rs1 011 rd 01011 11' => AMO(AMOMAXU_D(aq, rl, rd, rs1, rs2))

     case 'csr                rs1 001 rd 11100 11' => System( CSRRW(rd, rs1, csr))
     case 'csr                rs1 010 rd 11100 11' => System( CSRRS(rd, rs1, csr))
     case 'csr                rs1 011 rd 11100 11' => System( CSRRC(rd, rs1, csr))
     case 'csr                rs1 101 rd 11100 11' => System(CSRRWI(rd, rs1, csr))
     case 'csr                rs1 110 rd 11100 11' => System(CSRRSI(rd, rs1, csr))
     case 'csr                rs1 111 rd 11100 11' => System(CSRRCI(rd, rs1, csr))

     case '000000000000  00000 000 00000 11100 11' => System( ECALL)
     case '000000000001  00000 000 00000 11100 11' => System(EBREAK)
     case '100000000000  00000 000 00000 11100 11' => System(  ERET)

     -- unsupported instructions
     case _                                        => UnknownInstruction
   }

-- instruction printer

string imm(i::bits(N))  = "0x" : [i]
string instr(o::string) = PadRight(#" ", 12, o)

string amotype(aq::amo, rl::amo) =
    match aq, rl
    { case 0, 0 => ""
      case 1, 0 => ".aq"
      case 0, 1 => ".rl"
      case 1, 1 => ".sc"
    }

string pRtype(o::string, rd::reg, rs1::reg, rs2::reg) =
    instr(o) : " " : reg(rd) : ", " : reg(rs1) : ", " : reg(rs2)

string pARtype(o::string, aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
    pRtype([o : amotype(aq, rl)], rd, rs1, rs2)

string pLRtype(o::string, aq::amo, rl::amo, rd::reg, rs1::reg) =
    instr([o : amotype(aq, rl)]) : " " : reg(rd) : ", " : reg(rs1)

string pItype(o::string, rd::reg, rs1::reg, i::bits(N)) =
    instr(o) : " " : reg(rd) : ", " : reg(rs1) : ", " : imm(i)

string pCSRtype(o::string, rd::reg, rs1::reg, csr::creg) =
    instr(o) : " " : reg(rd) : ", " : reg(rs1) : ", " : csrName(csr)

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
   { case Branch(  BEQ(rs1, rs2, imm))      => pSBtype("BEQ",  rs1, rs2, imm)
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

     case MulDiv(    MUL(rd, rs1, rs2))     => pRtype("MUL",     rd, rs1, rs2)
     case MulDiv(   MULH(rd, rs1, rs2))     => pRtype("MULH",    rd, rs1, rs2)
     case MulDiv( MULHSU(rd, rs1, rs2))     => pRtype("MULHSU",  rd, rs1, rs2)
     case MulDiv(  MULHU(rd, rs1, rs2))     => pRtype("MULHU",   rd, rs1, rs2)
     case MulDiv(    DIV(rd, rs1, rs2))     => pRtype("DIV",     rd, rs1, rs2)
     case MulDiv(   DIVU(rd, rs1, rs2))     => pRtype("DIVU",    rd, rs1, rs2)
     case MulDiv(    REM(rd, rs1, rs2))     => pRtype("REM",     rd, rs1, rs2)
     case MulDiv(   REMU(rd, rs1, rs2))     => pRtype("REMU",    rd, rs1, rs2)

     case MulDiv(   MULW(rd, rs1, rs2))     => pRtype("MULW",    rd, rs1, rs2)
     case MulDiv(   DIVW(rd, rs1, rs2))     => pRtype("DIVW",    rd, rs1, rs2)
     case MulDiv(  DIVUW(rd, rs1, rs2))     => pRtype("DIVUW",   rd, rs1, rs2)
     case MulDiv(   REMW(rd, rs1, rs2))     => pRtype("REMW",    rd, rs1, rs2)
     case MulDiv(  REMUW(rd, rs1, rs2))     => pRtype("REMUW",   rd, rs1, rs2)

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

     case   FENCE(rd, rs1, pred, succ)      => pN0type("FENCE")
     case FENCE_I(rd, rs1, imm)             => pN0type("FENCE.I")

     case AMO(     LR_W(aq, rl, rd, rs1))       => pLRtype("LR.W",      aq, rl, rd, rs1)
     case AMO(     LR_D(aq, rl, rd, rs1))       => pLRtype("LR.D",      aq, rl, rd, rs1)
     case AMO(     SC_W(aq, rl, rd, rs1, rs2))  => pARtype("SC.W",      aq, rl, rd, rs1, rs2)
     case AMO(     SC_D(aq, rl, rd, rs1, rs2))  => pARtype("SC.D",      aq, rl, rd, rs1, rs2)

     case AMO(AMOSWAP_W(aq, rl, rd, rs1, rs2))  => pARtype("AMOSWAP.W", aq, rl, rd, rs1, rs2)
     case AMO( AMOADD_W(aq, rl, rd, rs1, rs2))  => pARtype("AMOADD.W",  aq, rl, rd, rs1, rs2)
     case AMO( AMOXOR_W(aq, rl, rd, rs1, rs2))  => pARtype("AMOXOR.W",  aq, rl, rd, rs1, rs2)
     case AMO( AMOAND_W(aq, rl, rd, rs1, rs2))  => pARtype("AMOAND.W",  aq, rl, rd, rs1, rs2)
     case AMO(  AMOOR_W(aq, rl, rd, rs1, rs2))  => pARtype("AMOOR.W",   aq, rl, rd, rs1, rs2)
     case AMO( AMOMIN_W(aq, rl, rd, rs1, rs2))  => pARtype("AMOMIN.W",  aq, rl, rd, rs1, rs2)
     case AMO( AMOMAX_W(aq, rl, rd, rs1, rs2))  => pARtype("AMOMAX.W",  aq, rl, rd, rs1, rs2)
     case AMO(AMOMINU_W(aq, rl, rd, rs1, rs2))  => pARtype("AMOMINU.W", aq, rl, rd, rs1, rs2)
     case AMO(AMOMAXU_W(aq, rl, rd, rs1, rs2))  => pARtype("AMOMAXU.W", aq, rl, rd, rs1, rs2)

     case AMO(AMOSWAP_D(aq, rl, rd, rs1, rs2))  => pARtype("AMOSWAP.D", aq, rl, rd, rs1, rs2)
     case AMO( AMOADD_D(aq, rl, rd, rs1, rs2))  => pARtype("AMOADD.D",  aq, rl, rd, rs1, rs2)
     case AMO( AMOXOR_D(aq, rl, rd, rs1, rs2))  => pARtype("AMOXOR.D",  aq, rl, rd, rs1, rs2)
     case AMO( AMOAND_D(aq, rl, rd, rs1, rs2))  => pARtype("AMOAND.D",  aq, rl, rd, rs1, rs2)
     case AMO(  AMOOR_D(aq, rl, rd, rs1, rs2))  => pARtype("AMOOR.D",   aq, rl, rd, rs1, rs2)
     case AMO( AMOMIN_D(aq, rl, rd, rs1, rs2))  => pARtype("AMOMIN.D",  aq, rl, rd, rs1, rs2)
     case AMO( AMOMAX_D(aq, rl, rd, rs1, rs2))  => pARtype("AMOMAX.D",  aq, rl, rd, rs1, rs2)
     case AMO(AMOMINU_D(aq, rl, rd, rs1, rs2))  => pARtype("AMOMINU.D", aq, rl, rd, rs1, rs2)
     case AMO(AMOMAXU_D(aq, rl, rd, rs1, rs2))  => pARtype("AMOMAXU.D", aq, rl, rd, rs1, rs2)

     case System( ECALL)                    => pN0type("ECALL")
     case System(EBREAK)                    => pN0type("EBREAK")
     case System(  ERET)                    => pN0type("ERET")

     case System( CSRRW(rd, rs1, csr))      => pCSRtype("CSRRW",  rd, rs1, csr)
     case System( CSRRS(rd, rs1, csr))      => pCSRtype("CSRRS",  rd, rs1, csr)
     case System( CSRRC(rd, rs1, csr))      => pCSRtype("CSRRC",  rd, rs1, csr)
     case System(CSRRWI(rd, rs1, csr))      => pCSRtype("CSRRWI", rd, rs1, csr)
     case System(CSRRSI(rd, rs1, csr))      => pCSRtype("CSRRSI", rd, rs1, csr)
     case System(CSRRCI(rd, rs1, csr))      => pCSRtype("CSRRCI", rd, rs1, csr)

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

bits(7) amofunc(code::bits(5), aq::amo, rl::amo) =
    code : aq : rl

word Encode(i::instruction) =
   match i
   { case Branch(  BEQ(rs1, rs2, imm))      => SBtype(opc(0x18), 0, rs1, rs2, imm)
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

     case MulDiv(    MUL(rd, rs1, rs2))     =>  Rtype(opc(0x0C), 0, rd, rs1, rs2, '0000001')
     case MulDiv(   MULH(rd, rs1, rs2))     =>  Rtype(opc(0x0C), 1, rd, rs1, rs2, '0000001')
     case MulDiv( MULHSU(rd, rs1, rs2))     =>  Rtype(opc(0x0C), 2, rd, rs1, rs2, '0000001')
     case MulDiv(  MULHU(rd, rs1, rs2))     =>  Rtype(opc(0x0C), 3, rd, rs1, rs2, '0000001')
     case MulDiv(    DIV(rd, rs1, rs2))     =>  Rtype(opc(0x0C), 4, rd, rs1, rs2, '0000001')
     case MulDiv(   DIVU(rd, rs1, rs2))     =>  Rtype(opc(0x0C), 5, rd, rs1, rs2, '0000001')
     case MulDiv(    REM(rd, rs1, rs2))     =>  Rtype(opc(0x0C), 6, rd, rs1, rs2, '0000001')
     case MulDiv(   REMU(rd, rs1, rs2))     =>  Rtype(opc(0x0C), 7, rd, rs1, rs2, '0000001')

     case MulDiv(   MULW(rd, rs1, rs2))     =>  Rtype(opc(0x0E), 0, rd, rs1, rs2, '0000001')
     case MulDiv(   DIVW(rd, rs1, rs2))     =>  Rtype(opc(0x0E), 4, rd, rs1, rs2, '0000001')
     case MulDiv(  DIVUW(rd, rs1, rs2))     =>  Rtype(opc(0x0E), 5, rd, rs1, rs2, '0000001')
     case MulDiv(   REMW(rd, rs1, rs2))     =>  Rtype(opc(0x0E), 6, rd, rs1, rs2, '0000001')
     case MulDiv(  REMUW(rd, rs1, rs2))     =>  Rtype(opc(0x0E), 7, rd, rs1, rs2, '0000001')

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

     case   FENCE(rd, rs1, pred, succ)      =>  Itype(opc(0x03), 0, rd, rs1, '0000' : pred : succ)
     case FENCE_I(rd, rs1, imm)             =>  Itype(opc(0x03), 1, rd, rs1, imm)

     case AMO(     LR_W(aq, rl, rd, rs1))       => Rtype(opc(0x0B), 2, rd, rs1, 0,   amofunc('00010', aq, rl))
     case AMO(     LR_D(aq, rl, rd, rs1))       => Rtype(opc(0x0B), 3, rd, rs1, 0,   amofunc('00010', aq, rl))
     case AMO(     SC_W(aq, rl, rd, rs1, rs2))  => Rtype(opc(0x0B), 2, rd, rs1, rs2, amofunc('00011', aq, rl))
     case AMO(     SC_D(aq, rl, rd, rs1, rs2))  => Rtype(opc(0x0B), 3, rd, rs1, rs2, amofunc('00010', aq, rl))

     case AMO(AMOSWAP_W(aq, rl, rd, rs1, rs2))  => Rtype(opc(0x0B), 2, rd, rs1, rs2, amofunc('00001', aq, rl))
     case AMO( AMOADD_W(aq, rl, rd, rs1, rs2))  => Rtype(opc(0x0B), 2, rd, rs1, rs2, amofunc('00000', aq, rl))
     case AMO( AMOXOR_W(aq, rl, rd, rs1, rs2))  => Rtype(opc(0x0B), 2, rd, rs1, rs2, amofunc('00100', aq, rl))
     case AMO( AMOAND_W(aq, rl, rd, rs1, rs2))  => Rtype(opc(0x0B), 2, rd, rs1, rs2, amofunc('01100', aq, rl))
     case AMO(  AMOOR_W(aq, rl, rd, rs1, rs2))  => Rtype(opc(0x0B), 2, rd, rs1, rs2, amofunc('01000', aq, rl))
     case AMO( AMOMIN_W(aq, rl, rd, rs1, rs2))  => Rtype(opc(0x0B), 2, rd, rs1, rs2, amofunc('10000', aq, rl))
     case AMO( AMOMAX_W(aq, rl, rd, rs1, rs2))  => Rtype(opc(0x0B), 2, rd, rs1, rs2, amofunc('10100', aq, rl))
     case AMO(AMOMINU_W(aq, rl, rd, rs1, rs2))  => Rtype(opc(0x0B), 2, rd, rs1, rs2, amofunc('11000', aq, rl))
     case AMO(AMOMAXU_W(aq, rl, rd, rs1, rs2))  => Rtype(opc(0x0B), 2, rd, rs1, rs2, amofunc('11100', aq, rl))

     case AMO(AMOSWAP_D(aq, rl, rd, rs1, rs2))  => Rtype(opc(0x0B), 3, rd, rs1, rs2, amofunc('00001', aq, rl))
     case AMO( AMOADD_D(aq, rl, rd, rs1, rs2))  => Rtype(opc(0x0B), 3, rd, rs1, rs2, amofunc('00000', aq, rl))
     case AMO( AMOXOR_D(aq, rl, rd, rs1, rs2))  => Rtype(opc(0x0B), 3, rd, rs1, rs2, amofunc('00100', aq, rl))
     case AMO( AMOAND_D(aq, rl, rd, rs1, rs2))  => Rtype(opc(0x0B), 3, rd, rs1, rs2, amofunc('01100', aq, rl))
     case AMO(  AMOOR_D(aq, rl, rd, rs1, rs2))  => Rtype(opc(0x0B), 3, rd, rs1, rs2, amofunc('01000', aq, rl))
     case AMO( AMOMIN_D(aq, rl, rd, rs1, rs2))  => Rtype(opc(0x0B), 3, rd, rs1, rs2, amofunc('10000', aq, rl))
     case AMO( AMOMAX_D(aq, rl, rd, rs1, rs2))  => Rtype(opc(0x0B), 3, rd, rs1, rs2, amofunc('10100', aq, rl))
     case AMO(AMOMINU_D(aq, rl, rd, rs1, rs2))  => Rtype(opc(0x0B), 3, rd, rs1, rs2, amofunc('11000', aq, rl))
     case AMO(AMOMAXU_D(aq, rl, rd, rs1, rs2))  => Rtype(opc(0x0B), 3, rd, rs1, rs2, amofunc('11100', aq, rl))

     case System( ECALL)                    =>  Itype(opc(0x1C), 0, 0, 0, 0)
     case System(EBREAK)                    =>  Itype(opc(0x1C), 0, 0, 0, 1)
     case System(  ERET)                    =>  Itype(opc(0x1C), 0, 0, 0, 0x800)

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

unit initMem(val::regType) = VMEM <- InitMap(val)

---------------------------------------------------------------------------
-- The next state function
---------------------------------------------------------------------------

string log_instruction(w::word, inst::instruction) =
    "instr " : [procID] : " " : [instCnt] :
    " 0x" : hex64(PC) : " : " : hex32(w) : "   " : instructionToString(inst)

declare done :: bool   -- Flag to request termination

unit incrCounts() =
{ UCSR.cycle    <- UCSR.cycle + 1
; UCSR.time     <- UCSR.time + 1
; UCSR.instret  <- UCSR.instret + 1
}

unit Next =
{ clear_logs()

; match Fetch()
  { case Some(w) =>
    { inst = Decode(w)
    ; mark_log(1, log_instruction(w, inst))
    ; Run(inst)
    }
    case None => nothing
  }

-- Handle the char i/o section of the Berkeley HTIF protocol
-- following cissrStandalone.c.
; when MCSR.mtohost <> 0x0
  do   { mark_log(0, log_tohost(MCSR.mtohost))
       ; MCSR.mtohost <- 0x0
       }

-- XXX: Definition of instret count is not clear in the case of
-- exceptions and traps.

; match Exception, BranchTo
  { case None, None       =>
             { PC <- PC + 4
             ; incrCounts ()
             }
    case None, Some(addr) =>
             { BranchTo <- None
             ; PC <- addr
             ; incrCounts ()
             }
    case Some(e), _       =>
             { mark_log(0, "Exception: " : [excName(e)])
             -- TODO
             ; nothing
             }
  }
}

unit initIdent(arch::Architecture) =
{ MCSR.mcpuid.ArchBase <- archBase(arch)
; MCSR.mcpuid.U        <- true
; MCSR.mcpuid.S        <- true
; MCSR.mcpuid.M        <- true
; MCSR.mcpuid.I        <- true

; MCSR.mimpid.RVSource <- 0x8000 -- anonymous source
; MCSR.mimpid.RVImpl   <- 0x0
}

unit initMachine() =
{ -- Startup in Mbare machine mode, with interrupts disabled.
  MCSR.mstatus.VM   <- vmMode(Mbare)
; MCSR.mstatus.MPRV <- privLevel(Machine)
; MCSR.mstatus.MIE  <- false

}
-- This initializes each core (via setting procID appropriately) on
-- startup before execution begins.
unit initRegs(pc::nat, stack::nat) =
{ -- TODO: Check if the specs specify the initial values of the registers
  -- on startup.  Initializing to an arbitrary value causes issues with
  -- the verifier, which assumes 0-valued initialization.
  for i in 0 .. 31 do
    gpr([i])   <- 0x0

; gpr([STACK]) <- [stack]

; PC           <- [pc]
; BranchTo     <- None
; Exception    <- None
; done         <- false
}
