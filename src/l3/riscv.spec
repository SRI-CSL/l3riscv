---------------------------------------------------------------------------
--
-- RISC-V Model
-- Based on the MIPS specification by Anthony Fox, University of Cambridge
--
-- Copyright (C) 2014, 2015 Anthony Fox, University of Cambridge
-- Copyright (C) 2014, 2015 Alexandre Joannou, University of Cambridge
-- Copyright (C) 2015-2018  SRI International.
--
-- This software was developed by SRI International and the University
-- of Cambridge Computer Laboratory under DARPA/AFRL contract
-- FA8750-11-C-0249 ("MRC2"), as part of the DARPA MRC research
-- programme, and under DARPA/AFRL contract FA8750-10-C-0237
-- ("CTSRD"), as part of the DARPA CRASH research programme, and under
-- DARPA/AFRL contract FA8650-18-C-7809 ("CIFV").
--
-- See the LICENSE file for details.
--
-- For syntax highlighting, treat this file as Haskell source.
---------------------------------------------------------------------------


---------------------------------------------------------------------------
-- Basic types
---------------------------------------------------------------------------

type id       = bits(8)         -- max 256 cores
type reg      = bits(5)
type csreg    = bits(12)        -- CSR address space

type byte     = bits(8)
type half     = bits(16)
type word     = bits(32)
type dword    = bits(64)

type fprnd    = bits(3)         -- rounding mode
type fpval    = bits(64)

type exc_code = bits(4)

-- instruction fields
type opcode   = bits(7)
type imm12    = bits(12)
type imm20    = bits(20)
type amo      = bits(1)

-- floating point types

construct fpType { FP_single, FP_double }

-- memory and caches

construct accessType { Read, Write, ReadWrite, Execute }
construct fetchType  { Instruction, Data }

type asid32   = bits(9)
type asid64   = bits(16)

-- RV64* base.

type regType  = dword
type vAddr    = dword
type pAddr    = dword

type pAddrIdx = bits(61)        -- raw index into physical memory
                                -- arranged in 8-byte blocks

-- Miscellaneous
exception UNDEFINED         :: string
exception INTERNAL_ERROR    :: string

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

type arch_xlen = bits(2)

construct Architecture
{
  RV32, RV64, RV128
}

arch_xlen archBase(a::Architecture) =
    match a
    { case RV32       => 1
      case RV64       => 2
      case RV128      => 3
    }

Architecture architecture(xlen::arch_xlen) =
    match xlen
    { case 1          => RV32
      case 2          => RV64
      case 3          => RV128
      case _          => #UNDEFINED("Unknown architecture: " : [[xlen] :: nat])
    }

---------------------------------------------------------------------------
-- Privilege levels
---------------------------------------------------------------------------

type priv_level = bits(2)

construct Privilege
{ User
, Supervisor
, Machine
}

priv_level privLevel(p::Privilege) =
    match p
    { case User       => 0
      case Supervisor => 1
      case Machine    => 3
    }

Privilege privilege(p::priv_level) =
    match p
    { case 0          => User
      case 1          => Supervisor
      case 3          => Machine
      case _          => #UNDEFINED("Unknown privilege: " : [p])
    }

string privName(p::Privilege) =
    match p
    { case User       => "U"
      case Supervisor => "S"
      case Machine    => "M"
    }

---------------------------------------------------------------------------
-- S-mode address translation and page protection
---------------------------------------------------------------------------

type satp_mode  = bits(4)

construct SATP_Mode
{ Sbare
, Sv32
, Sv39
, Sv48
-- todo: Sv57, Sv64
}

SATP_Mode satpMode_ofbits(m::satp_mode, a::Architecture) =
    match m, a
    { case  0, _    => Sbare
      case  1, RV32 => Sv32
      case  8, RV64 => Sv39
      case  9, RV64 => Sv48
      -- todo: Sv57, Sv64
      case  _     => #UNDEFINED("Unknown address translation mode: " : [[m]::nat])
    }

satp_mode satpMode_tobits(m::SATP_Mode, a::Architecture) =
    match m, a
    { case Sbare, _   => 0
      case Sv32, RV32 => 1
      case Sv39, RV64 => 8
      case Sv48, RV64 => 9
      -- todo: Sv57, Sv64
      case  _     => #UNDEFINED("Unsupported address translation mode: "
                                : [m]::string : " in " : [a]::string)
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

ext_status ext_status(e::ExtStatus) =
    match e
    { case Off      => 0
      case Initial  => 1
      case Clean    => 2
      case Dirty    => 3
    }

ExtStatus extStatus(e::ext_status) =
    match e
    { case 0        => Off
      case 1        => Initial
      case 2        => Clean
      case 3        => Dirty
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

construct ExceptionType
{ E_Fetch_Addr_Align    --  0
, E_Fetch_Access_Fault  --  1
, E_Illegal_Instr       --  2
, E_Breakpoint          --  3
, E_Load_Addr_Align     --  4
, E_Load_Access_Fault   --  5
, E_SAMO_Addr_Align     --  6
, E_SAMO_Access_Fault   --  7
, E_U_EnvCall           --  8
, E_S_EnvCall           --  9
, E_Reserved_10
, E_M_EnvCall           -- 11
, E_Fetch_Page_Fault    -- 12
, E_Load_Page_Fault     -- 13
, E_Reserved_14
, E_SAMO_Page_Fault     -- 15
}

construct InterruptType
{ I_U_Software
, I_S_Software
, I_M_Software
, I_U_Timer
, I_S_Timer
, I_M_Timer
, I_U_External
, I_S_External
, I_M_External
}

-- interrupts are sparser than exceptions so we'll use an explicit
-- cast
exc_code interruptIndex(i::InterruptType) =
    match i
    { case I_U_Software => 0x0
      case I_S_Software => 0x1
      case I_M_Software => 0x3

      case I_U_Timer    => 0x4
      case I_S_Timer    => 0x5
      case I_M_Timer    => 0x7

      case I_U_External => 0x8
      case I_S_External => 0x9
      case I_M_External => 0xb
    }

---------------------------------------------------------------------------
-- Control and Status Registers (CSRs)
---------------------------------------------------------------------------

-- Machine state projections
--
-- There are two kinds of projections needed: (i) from machine-level
-- views to views from lower privilege levels, and (ii) from the
-- 64-bit implementation width to 32-bit views.  So, for e.g.
--
--   mstatus-64  ->  sstatus-64  ->  ustatus-64
--      |               |               |
--      V               V               V
--   mstatus-32  ->  sstatus-32  ->  ustatus-32
--
-- These two kinds of projections will be called 'lowering' below, and
-- ideally, the two kinds should commute.  These projections are used
-- when reading from the underlying 64-bit implementation of the CSR.
-- Projections going in the other direction are needed when writing a
-- value to the CSR, and will be called 'lifting' below.
--
-- In addition, several fields in machine state registers are WARL or
-- WLRL, requiring that values written to the registers be legalized.
-- For each such register, there will be an associated 'legalize_'
-- function.  These functions will need to be supplied externally, and
-- will depend on the legal values supported by an implementation (or
-- misa).  The legalize_ functions generate a legal value from the
-- current value and the written value.  In more complex cases, they
-- will also implicitly read the current values of misa, mstatus, etc.
--
-- Each register definition below is followed by custom projections
-- and choice of legalizations if needed.  For now, we typically
-- implement the simplest legalize_ alternatives.
--
-- TODO: writes to WPRI fields are not currently checked for
-- preservation; adding checks would provide useful diagnostics.

-- Machine-Level CSRs

register misa :: regType        -- Machine ISA
{ 63-62 : MXL       -- machine XLEN
,    23 : X         -- non-standard extensions
,    20 : U         -- user-mode
,    18 : S         -- supervisor-mode
,    13 : N         -- user-level interrupts
,    12 : M         -- integer multiply/divide
,     8 : I         -- RV32I/RV64I base ISA
,     7 : H         -- hypervisor
,     6 : G         -- additional standard extensions
,     5 : F         -- single-precision floating-point
,     3 : D         -- double-precision floating-point
,     2 : C         -- compressed (RVC)
,     0 : A         -- atomics
}

word  isa_to_32(v::dword) = [v<63:62> : 0x0`4  : v<25:0>]
dword isa_of_32(v::word)  = [v<31:30> : 0x0`36 : v<25:0>]

misa legalize_misa_64(m::misa, v::regType) =
    -- For now, we ignore all writes.
    m

misa legalize_misa_32(m::misa, v::word) =
    legalize_misa_64(m, isa_of_32(v))

regType MVENDORID = 0
regType MARCHID   = 0
regType MIMPID    = 0


register mstatus :: regType     -- Machine Status
{    63 : M_SD      -- extended context dirty status
, 35-34 : M_SXL     -- effective XLEN/base ISA in S-mode
, 33-32 : M_UXL     -- effective XLEN/base ISA in U-mode
,    22 : M_TSR     -- trap sret
,    21 : M_TW      -- timeout wait
,    20 : M_TVM     -- trap virtual memory
,    19 : M_MXR     -- make executable readable
,    18 : M_SUM     -- permit supervisor access to user memory
,    17 : M_MPRV    -- load/store memory privilege
, 16-15 : M_XS      -- extension context status
, 14-13 : M_FS      -- floating-point context status
, 12-11 : M_MPP     -- per-privilege pre-trap privilege modes
,     8 : M_SPP
,     7 : M_MPIE    -- per-privilege pre-trap interrupt enables
,     5 : M_SPIE
,     4 : M_UPIE
,     3 : M_MIE     -- per-privilege interrupt enables
,     1 : M_SIE
,     0 : M_UIE
}

word  status_to_32(v::dword) = [v<63>]::bits(1) : 0x0`2  : v<28:0>
dword status_of_32(v::word)  = [v<31>]::bits(1) : 0x0`32 : v<30:0>

mstatus legalize_mstatus_64(m::mstatus, v::regType) =
{ var ms = mstatus(v)

-- We don't have any extension context yet.
; ms.M_XS   <- ext_status(Off)
; ms.M_SD   <- extStatus(ms.M_FS) == Dirty or extStatus(ms.M_XS) == Dirty

-- For now, we don't allow SXL and UXL to be changed, for Spike compatibility.
; ms.M_SXL  <- m.M_SXL
; ms.M_UXL  <- m.M_UXL

-- Hardwired to zero in the absence of 'N'.
; ms.M_UPIE <- false
; ms.M_UIE  <- false
; ms
}

mstatus legalize_mstatus_32(m::mstatus, v::word) =
    legalize_mstatus_64(m, status_of_32(v))


construct TrapVectorMode
{ TV_Direct
, TV_Vector
, TV_Reserved1
, TV_Reserved2
}

register mtvec :: regType       -- Trap-Vector Base-Address
{  63-2 : BASE
,   1-0 : MODE
}

word  tvec_to_32(v::dword) = v<31:0>
dword tvec_of_32(v::word)  = ZeroExtend(v)  -- FIXME: Is this correct?

mtvec legalize_tvec_64(m::mtvec, v::regType) =
{ var mtv = mtvec(v)
; match [mtv.MODE]::TrapVectorMode
  { case TV_Direct => mtv
    case TV_Vector => mtv
    case _     => { mtv.MODE <- m.MODE
                  ; mtv
                  }
  }
}

mtvec legalize_tvec_32(m::mtvec, v::word) =
    legalize_tvec_64(m, tvec_of_32(v))


register medeleg :: regType     -- Exception Trap Delegation
{    15 : M_SAMO_Page_Fault
,    13 : M_Load_Page_Fault
,    12 : M_Fetch_Page_Fault
,    11 : M_MEnvCall
,     9 : M_SEnvCall
,     8 : M_UEnvCall
,     7 : M_SAMO_Access_Fault
,     6 : M_SAMO_Addr_Align
,     5 : M_Load_Access_Fault
,     4 : M_Load_Addr_Align
,     3 : M_Breakpoint
,     2 : M_Illegal_Instr
,     1 : M_Fetch_Access_Fault
,     0 : M_Fetch_Addr_Align
}

-- shared between {m,s}{i,e}deleg
word  deleg_to_32(v::dword) = v<31:0>
dword deleg_of_32(v::word)  = ZeroExtend(v)

medeleg legalize_medeleg_64(m::medeleg, v::regType) =
{ var md = medeleg(v)
; md.M_MEnvCall <- false    -- cannot delegate M-mode EnvCalls
; md
}

medeleg legalize_medeleg_32(m::medeleg, v::word) =
    legalize_medeleg_64(m, deleg_of_32(v))


register mideleg :: regType     -- Interrupt Trap Delegation
{    11 : M_MEIP   -- external interrupts
,     9 : M_SEIP
,     8 : M_UEIP
,     7 : M_MTIP   -- timer interrupts
,     5 : M_STIP
,     4 : M_UTIP
,     3 : M_MSIP   -- software interrupts
,     1 : M_SSIP
,     0 : M_USIP
}

-- just a stub hook for now
mideleg legalize_mideleg_64(m::mideleg, v::regType) =
    mideleg(v)

mideleg legalize_mideleg_32(m::mideleg, v::word) =
    legalize_mideleg_64(m, deleg_of_32(v))


register mip :: regType         -- Interrupt Pending
{    11 : M_MEIP   -- external interrupts
,     9 : M_SEIP
,     8 : M_UEIP
,     7 : M_MTIP   -- timer interrupts
,     5 : M_STIP
,     4 : M_UTIP
,     3 : M_MSIP   -- software interrupts
,     1 : M_SSIP
,     0 : M_USIP
}

-- shared across {m,s}i{p,e}
word  ipe_to_32(v::dword) = v<31:0>
dword ipe_of_32(v::word)  = ZeroExtend(v<11:0>)

mip legalize_mip_64(mip::mip, v::regType) =
{ var m = mip
; v     = mip(v)
-- MTIP, MEIP and MSIP are read-only for M-mode CSR writes to mip, and
-- are controlled via writes to memory-mapped control registers.
; m.M_SEIP <- v.M_SEIP
; m.M_STIP <- v.M_STIP
; m.M_SSIP <- v.M_SSIP

-- hardwired to 0 since 'N' is yet not supported
-- ; m.M_UEIP <- v.M_UEIP
-- ; m.M_UTIP <- v.M_UTIP
-- ; m.M_USIP <- v.M_USIP
; m
}

mip legalize_mip_32(m::mip, v::word) =
    legalize_mip_64(m, ipe_of_32(v))


register mie :: regType         -- Interrupt Enable
{    11 : M_MEIE    -- external interrupts
,     9 : M_SEIE
,     8 : M_UEIE
,     7 : M_MTIE    -- timer interrupts
,     5 : M_STIE
,     4 : M_UTIE
,     3 : M_MSIE    -- software interrupts
,     1 : M_SSIE
,     0 : M_USIE
}

mie legalize_mie_64(mie::mie, v::regType) =
{ var m = mie
; v     = mie(v)
; m.M_MEIE <- v.M_MEIE
; m.M_MTIE <- v.M_MTIE
; m.M_MSIE <- v.M_MSIE
; m.M_SEIE <- v.M_SEIE
; m.M_STIE <- v.M_STIE
; m.M_SSIE <- v.M_SSIE

-- hardwired to 0 since 'N' is yet not supported
-- ; m.M_UEIE <- v.M_UEIE
-- ; m.M_UTIE <- v.M_UTIE
-- ; m.M_USIE <- v.M_USIE

; m
}

mie legalize_mie_32(m::mie, v::word) =
    legalize_mie_64(m, ipe_of_32(v))


register mcounteren :: word  -- Machine Counter-Enable
{    31 : M_HPM31
,    30 : M_HPM30
,    29 : M_HPM29
,    28 : M_HPM28
,    27 : M_HPM27
,    26 : M_HPM26
,    25 : M_HPM25
,    24 : M_HPM24
,    23 : M_HPM23
,    22 : M_HPM22
,    21 : M_HPM21
,    20 : M_HPM20
,    19 : M_HPM19
,    18 : M_HPM18
,    17 : M_HPM17
,    16 : M_HPM16
,    15 : M_HPM15
,    14 : M_HPM14
,    13 : M_HPM13
,    12 : M_HPM12
,    11 : M_HPM11
,    10 : M_HPM10
,     9 : M_HPM9
,     8 : M_HPM8
,     7 : M_HPM7
,     6 : M_HPM6
,     5 : M_HPM5
,     4 : M_HPM4
,     3 : M_HPM3
,     2 : M_IR      -- instructions retired
,     1 : M_TM      -- time
,     0 : M_CY      -- cycles
}


register mcause :: regType      -- Trap Cause
{    63 : M_Intr
,  62-0 : M_ExcCause
}

word  cause_to_32(v::dword) = [v<63>]::bits(1) : v<30:0>
dword cause_of_32(v::word)  = [v<31>]::bits(1) : 0x0`32 : v<30:0>

mcause legalize_mcause_64(m::mcause, v::regType) =
    mcause(v)

mcause legalize_mcause_32(m::mcause, v::word) =
    legalize_mcause_64(m, cause_of_32(v))


record MachineCSR
{ mvendorid     :: regType      -- information registers
  marchid       :: regType
  mimpid        :: regType
  mhartid       :: regType

  mstatus       :: mstatus      -- trap setup
  misa          :: misa
  medeleg       :: medeleg
  mideleg       :: mideleg
  mie           :: mie
  mtvec         :: mtvec
  mcounteren    :: mcounteren

  mscratch      :: regType      -- trap handling
  mepc          :: regType
  mcause        :: mcause
  mtval         :: regType
  mip           :: mip

  mcycle        :: regType      -- counters
  minstret      :: regType

  mtime         :: regType         -- this is memory-mapped and not a
                                   -- CSR, but put here for now
}

-- Supervisor-Level CSRs

register sstatus :: regType
{    63 : S_SD      -- extended context dirty status
, 33-32 : S_UXL     -- permit supervisor user-memory access
,    19 : S_MXR     -- make executable readable
,    18 : S_SUM     -- permit supervisor access to user memory
, 16-15 : S_XS      -- extension context status
, 14-13 : S_FS      -- floating-point context status
,     8 : S_SPP     -- pre-trap privilege modes
,     5 : S_SPIE    -- pre-trap interrupt enables
,     4 : S_UPIE
,     1 : S_SIE     -- interrupt-enables
,     0 : S_UIE
}

sstatus lower_mstatus(m::mstatus) =
{ var s = sstatus(0)
; s.S_SD    <- m.M_SD
; s.S_UXL   <- m.M_UXL
; s.S_MXR   <- m.M_MXR
; s.S_SUM   <- m.M_SUM
; s.S_XS    <- m.M_XS
; s.S_FS    <- m.M_FS
; s.S_SPP   <- m.M_SPP
; s.S_SPIE  <- m.M_SPIE
; s.S_UPIE  <- m.M_UPIE
; s.S_SIE   <- m.M_SIE
; s.S_UIE   <- m.M_UIE
; s
}

mstatus lift_sstatus(ctx::mstatus, s::sstatus) =
{ var m = mstatus(&ctx)
; m.M_SD    <- s.S_SD
; m.M_UXL   <- s.S_UXL
; m.M_MXR   <- s.S_MXR
; m.M_SUM   <- s.S_SUM
; m.M_XS    <- s.S_XS
; m.M_FS    <- s.S_FS
; m.M_SPP   <- s.S_SPP
; m.M_SPIE  <- s.S_SPIE
; m.M_UPIE  <- s.S_UPIE
; m.M_SIE   <- s.S_SIE
; m.M_UIE   <- s.S_UIE
; m
}

mstatus legalize_sstatus_64(m::mstatus, v::regType) =
    lift_sstatus(m, sstatus(v))

mstatus legalize_sstatus_32(m::mstatus, v::word) =
    legalize_sstatus_64(m, status_of_32(v))


register sedeleg :: regType     -- Exception Trap Delegation
{     9 : S_SEnvCall
,     8 : S_UEnvCall
,     7 : S_SAMO_Access
,     6 : S_SAMO_Addr
,     5 : S_Load_Access
,     4 : S_Load_Addr_Align
,     3 : S_Breakpoint
,     2 : S_Illegal_Instr
,     1 : S_Fetch_Fault
,     0 : S_Fetch_Addr_Align
}

sedeleg legalize_sedeleg_64(s::sedeleg, v::regType) =
{ var sd = sedeleg(v)
; sd.S_SEnvCall <- false    -- cannot delegate S-mode EnvCalls
; sd
}

sedeleg legalize_sedeleg_32(s::sedeleg, v::word) =
    legalize_sedeleg_64(s, deleg_of_32(v))


register sideleg :: regType     -- Interrupt Trap Delegation
{     9 : S_SEIP   -- external interrupts
,     8 : S_UEIP
,     5 : S_STIP   -- timer interrupts
,     4 : S_UTIP
,     1 : S_SSIP   -- software interrupts
,     0 : S_USIP
}

sideleg legalize_sideleg_64(s::sideleg, v::regType) =
    -- nop for now
    sideleg(v)

sideleg legalize_sideleg_32(s::sideleg, v::word) =
    legalize_sideleg_64(s, deleg_of_32(v))

register sip :: regType         -- Interrupt Pending
{     9 : S_SEIP   -- external interrupts
,     8 : S_UEIP
,     5 : S_STIP   -- timer interrupts
,     4 : S_UTIP
,     1 : S_SSIP   -- software interrupts
,     0 : S_USIP
}

sip lower_mip(m::mip, d::mideleg) =
{ var s = sip(0)
; s.S_SEIP  <- m.M_SEIP and d.M_SEIP
; s.S_UEIP  <- m.M_UEIP and d.M_UEIP
; s.S_STIP  <- m.M_STIP and d.M_STIP
; s.S_UTIP  <- m.M_UTIP and d.M_UTIP
; s.S_SSIP  <- m.M_SSIP and d.M_SSIP
; s.S_USIP  <- m.M_USIP and d.M_USIP
; s
}

mip lift_sip(ctx::mip, s::sip, d::mideleg) =
{ var m = ctx
; when d.M_SEIP do m.M_SEIP <- s.S_SEIP
; when d.M_UEIP do m.M_UEIP <- s.S_UEIP
; when d.M_STIP do m.M_STIP <- s.S_STIP
; when d.M_UTIP do m.M_UTIP <- s.S_UTIP
; when d.M_SSIP do m.M_SSIP <- s.S_SSIP
; when d.M_USIP do m.M_USIP <- s.S_USIP
; m
}

mip legalize_sip_64(m::mip, d::mideleg, v::regType) =
    lift_sip(m, sip(v), d)

mip legalize_sip_32(m::mip, d::mideleg, v::word) =
    legalize_sip_64(m, d, ipe_of_32(v))


register sie :: regType         -- Interrupt Enable
{     9 : S_SEIE    -- external interrupts
,     8 : S_UEIE
,     5 : S_STIE    -- timer interrupts
,     4 : S_UTIE
,     1 : S_SSIE    -- software interrupts
,     0 : S_USIE
}

sie lower_mie(m::mie, d::mideleg) =
{ var s = sie(0)
; s.S_SEIE  <- m.M_SEIE and d.M_SEIP
; s.S_UEIE  <- m.M_UEIE and d.M_UEIP
; s.S_STIE  <- m.M_STIE and d.M_STIP
; s.S_UTIE  <- m.M_UTIE and d.M_UTIP
; s.S_SSIE  <- m.M_SSIE and d.M_SSIP
; s.S_USIE  <- m.M_USIE and d.M_USIP
; s
}

mie lift_sie(ctx::mie, s::sie, d::mideleg) =
{ var m = ctx
; when d.M_SEIP do m.M_SEIE <- s.S_SEIE
; when d.M_UEIP do m.M_UEIE <- s.S_UEIE
; when d.M_STIP do m.M_STIE <- s.S_STIE
; when d.M_UTIP do m.M_UTIE <- s.S_UTIE
; when d.M_SSIP do m.M_SSIE <- s.S_SSIE
; when d.M_USIP do m.M_USIE <- s.S_USIE
; m
}

mie legalize_sie_64(m::mie, d::mideleg, v::regType) =
    lift_sie(m, sie(v), d)

mie legalize_sie_32(m::mie, d::mideleg, v::word) =
    legalize_sie_64(m, d, ipe_of_32(v))


register satp32 :: word         -- Address Translation and Protection
{ 31    : SATP32_MODE
, 30-22 : SATP32_ASID
, 21-0  : SATP32_PPN
}

register satp64 :: regType
{ 63-60 : SATP64_MODE
, 59-44 : SATP64_ASID
, 43-0  : SATP64_PPN
}

-- TODO: legalization of writes to satp

record SupervisorCSR
{                               -- trap setup
  -- sstatus is a restricted view of mstatus
  sedeleg       :: sedeleg
  sideleg       :: sideleg
  -- sie is a restricted view of mie
  stvec         :: mtvec
  scounteren    :: mcounteren

  sscratch      :: regType      -- trap handling
  sepc          :: regType
  scause        :: mcause
  stval         :: regType
  -- sip is a restricted view of mip

  satp          :: regType      -- address translation and protection
}

-- User-Level CSRs

-- floating point control and status

register FPCSR :: word          -- 32-bit control register
{ 7-5 : FRM         -- dynamic rounding mode
                    -- exception flags
,   4 : NV          --     invalid operation
,   3 : DZ          --     divide by zero
,   2 : OF          --     overflow
,   1 : UF          --     underflow
,   0 : NX          --     inexact
}

-- FIXME: The registers below are not yet fully specified in the
-- privileged spec.  They will need updating once the 'N' extension is
-- specified.

register ustatus :: regType     -- Status
{     4 : U_PIE     -- pre-trap interrupt enable
,     0 : U_IE      -- interrupt-enable
}

ustatus lower_sstatus(v::sstatus) =
{ var u = ustatus(0)
; u.U_PIE   <- v.S_UPIE
; u.U_IE    <- v.S_UIE
; u
}

sstatus lift_ustatus(ctx::sstatus, u::ustatus) =
{ var s = sstatus(&ctx)
; s.S_UPIE  <- u.U_PIE
; s.S_UIE   <- u.U_IE
; s
}

sstatus legalize_ustatus_64(s::sstatus, v::regType) =
    lift_ustatus(s, ustatus(v))

sstatus legalize_ustatus_32(s::sstatus, v::word) =
    legalize_ustatus_64(s, status_of_32(v))


register uip :: regType         -- Interrupt Pending
{     8 : U_EIP     -- external interrupts
,     4 : U_TIP     -- timer interrupts
,     0 : U_SIP     -- software interrupts
}

uip lower_sip(s::sip, d::sideleg) =
{ var u = uip(0)
; u.U_EIP   <- s.S_UEIP and d.S_UEIP
; u.U_TIP   <- s.S_UTIP and d.S_UTIP
; u.U_SIP   <- s.S_USIP and d.S_USIP
; u
}

sip lift_uip(ctx::sip, u::uip, d::sideleg) =
{ var s = sip(&ctx)
; when d.S_UEIP do s.S_UEIP  <- u.U_EIP
; when d.S_UTIP do s.S_UTIP  <- u.U_TIP
; when d.S_USIP do s.S_USIP  <- u.U_SIP
; s
}

sip legalize_uip_64(s::sip, d::sideleg, v::regType) =
    lift_uip(s, uip(v), d)

sip legalize_uip_32(s::sip, d::sideleg, v::word) =
    legalize_uip_64(s, d, status_of_32(v))


register uie :: regType         -- Interrupt Enable
{     8 : U_EIE     -- external interrupts
,     4 : U_TIE     -- timer interrupts
,     0 : U_SIE     -- software interrupts
}

uie lower_sie(s::sie, d::sideleg) =
{ var u = uie(0)
; u.U_EIE   <- s.S_UEIE and d.S_UEIP
; u.U_TIE   <- s.S_UTIE and d.S_UTIP
; u.U_SIE   <- s.S_USIE and d.S_USIP
; u
}

sie lift_uie(ctx::sie, u::uie, d::sideleg) =
{ var s = sie(&ctx)
; when d.S_UEIP do s.S_UEIE  <- u.U_EIE
; when d.S_UTIP do s.S_UTIE  <- u.U_TIE
; when d.S_USIP do s.S_USIE  <- u.U_SIE
; s
}

sie legalize_uie_64(s::sie, d::sideleg, v::regType) =
    lift_uie(s, uie(v), d)

sie legalize_uie_32(s::sie, d::sideleg, v::word) =
    legalize_uie_64(s, d, ipe_of_32(v))

record UserCSR
{ utvec         :: mtvec        -- trap setup

  uscratch      :: regType      -- trap handling
  uepc          :: regType
  ucause        :: mcause
  utval         :: regType

  fpcsr         :: FPCSR        -- floating-point
}


-- utilities for privilege transitions

mstatus menter(v::mstatus, p::Privilege) =
{ var m = v
; m.M_MPIE <- m.M_MIE
; m.M_MIE  <- false
; m.M_MPP  <- privLevel(p)
; m
}

mstatus senter(v::mstatus, p::Privilege) =
{ var m = v
; m.M_SPIE <- m.M_SIE
; m.M_SIE  <- false
; match p
  { case User       => m.M_SPP  <- false
    case Supervisor => m.M_SPP  <- true
    case _          => #INTERNAL_ERROR("Invalid privilege for senter")
  }
; m
}

mstatus uenter(v::mstatus, p::Privilege) =
{ var m = v
; m.M_UPIE <- m.M_UIE
; m.M_UIE  <- false
; m
}

mstatus mret(v::mstatus) =
{ var m = v
; m.M_MIE  <- m.M_MPIE
; m.M_MPIE <- true
; m.M_MPP  <- privLevel(User)  -- todo: need to check config if U-mode is supported
; m
}

mstatus sret(v::mstatus) =
{ var m = v
; m.M_SIE  <- m.M_SPIE
; m.M_SPIE <- true
; m.M_SPP  <- false
; m
}

mstatus uret(v::mstatus) =
{ var m = v
; m.M_UIE  <- m.M_UPIE
; m.M_UPIE <- true
; m
}

---------------------------------------------------------------------------
-- Instruction fetch state
---------------------------------------------------------------------------

record SynchronousException
{ trap            :: ExceptionType
  badaddr         :: vAddr option
}

construct instrResult
{ Trap            :: SynchronousException
, Uret
, Sret
, Mret
, BranchTo        :: regType
}

type FetchState = instrResult option

---------------------------------------------------------------------------
-- Register state space
---------------------------------------------------------------------------

-- Each register state is local to a core.

type RegFile    = reg  -> regType

declare
{ clock         :: regType                      -- global clock and counters

  c_instret     :: id -> regType                -- per-core counters
  c_cycles      :: id -> regType

  c_gpr         :: id -> RegFile                -- general purpose registers
  c_fpr         :: id -> RegFile                -- floating-point registers

  c_PC          :: id -> regType                -- program counter

  c_UCSR        :: id -> UserCSR                -- user-level CSRs
  c_SCSR        :: id -> SupervisorCSR          -- supervisor-level CSRs
  c_MCSR        :: id -> MachineCSR             -- machine-level CSRs

  c_privilege   :: id -> Privilege              -- current execution context privilege

  -- interpreter execution context
  c_NextFetch   :: id -> FetchState
  c_ReserveLoad :: id -> vAddr option           -- load reservation for LL/SC
  c_ExitCode    :: id -> regType                -- derived from Berkeley HTIF
}

-- Number of cores
declare totalCore :: nat

-- ID of the core executing current instruction
declare procID :: id

unit scheduleCore(id :: nat) =
    when id < totalCore
    do procID <- [id]

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

component fcsr :: FPCSR
{ value        = c_UCSR(procID).fpcsr
  assign value = { c_UCSR(procID).fpcsr         <- value
                 ; c_MCSR(procID).mstatus.M_FS  <- ext_status(Dirty)
                 ; c_MCSR(procID).mstatus.M_SD  <- true
                 }
}

component fpr(n::reg) :: regType
{ value        = { m = c_fpr(procID); m(n) }
  assign value = { var m = c_fpr(procID)
                 ; m(n) <- value
                 ; c_fpr(procID) <- m
                 }
}

component PC :: regType
{ value        = c_PC(procID)
  assign value = c_PC(procID) <- value
}

component UCSR :: UserCSR
{ value        = c_UCSR(procID)
  assign value = c_UCSR(procID) <- value
}

component SCSR :: SupervisorCSR
{ value        = c_SCSR(procID)
  assign value = c_SCSR(procID) <- value
}

component MCSR :: MachineCSR
{ value        = c_MCSR(procID)
  assign value = c_MCSR(procID) <- value
}

component NextFetch :: FetchState
{ value        = c_NextFetch(procID)
  assign value = c_NextFetch(procID) <- value
}

component ReserveLoad :: vAddr option
{ value        = c_ReserveLoad(procID)
  assign value = c_ReserveLoad(procID) <- value
}

component ExitCode :: regType
{ value        = c_ExitCode(procID)
  assign value = c_ExitCode(procID) <- value
}

component curPrivilege :: Privilege
{ value        = c_privilege(procID)
  assign value = c_privilege(procID) <- value
}

-- machine state utilities

Architecture curArch() = architecture(MCSR.misa.MXL)

bool in32BitMode()  = curArch() == RV32

bool haveFPSingle() = MCSR.misa.F
bool haveFPDouble() = MCSR.misa.D

bool haveFP()       = MCSR.misa.F or MCSR.misa.D

asid32 curAsid32()  = satp32(SCSR.satp<31:0>).SATP32_ASID

asid64 curAsid64()  = satp64(SCSR.satp).SATP64_ASID

---------------------------------------------------------------------------
-- Floating Point
---------------------------------------------------------------------------

-- Rounding

construct Rounding
{ RNE, RTZ, RDN, RUP, RMM, RDYN }

-- instruction rounding mode
Rounding option rnd_mode_static(rnd::fprnd) =
    match rnd
    { case 0          => Some(RNE)
      case 1          => Some(RTZ)
      case 2          => Some(RDN)
      case 3          => Some(RUP)
      case 4          => Some(RMM)
      case 7          => Some(RDYN)     -- from rounding mode register
      case _          => None
    }

-- dynamic rounding mode
Rounding option rnd_mode_dynamic(rnd::fprnd) =
    match rnd
    { case 0          => Some(RNE)
      case 1          => Some(RTZ)
      case 2          => Some(RDN)
      case 3          => Some(RUP)
      case 4          => Some(RMM)
      case _          => None
    }

-- currently implemented rounding modes
ieee_rounding option l3round(rnd::Rounding) =
    match rnd
    { case RNE        => Some(roundTiesToEven)
      case RTZ        => Some(roundTowardZero)
      case RDN        => Some(roundTowardNegative)
      case RUP        => Some(roundTowardPositive)
      case RMM        => None  -- roundTiesToMaxMagnitude not in L3
      case RDYN       => None  -- invalid
    }

-- composed rounding mode
ieee_rounding option round(rnd::fprnd) =
    match rnd_mode_static(rnd)
    { case Some(RDYN) => match rnd_mode_dynamic(fcsr.FRM)
                         { case Some(frm) => l3round(frm)
                           case None      => None
                         }
      case Some(frm)  => l3round(frm)
      case None       => None
    }

-- NaNs

bits(32) RV32_CanonicalNan = 0x7fc00000
bits(64) RV64_CanonicalNan = 0x7ff8000000000000

-- Classification

bool FP32_IsSignalingNan(x::bits(32)) =
    (x<30:23> == 0xff`8)   and x<22> == false and (x<21:0> != 0x0`22)

bool FP64_IsSignalingNan(x::bits(64)) =
    (x<62:52> == 0x7ff`11) and x<51> == false and (x<50:0> != 0x0`51)

bool FP32_Sign(x::bits(32)) = x<31>
bool FP64_Sign(x::bits(64)) = x<63>

-- setting exception flags

unit setFP_Invalid() =
    fcsr.NV <- true

unit setFP_DivZero() =
    fcsr.DZ <- true

unit setFP_Overflow() =
    fcsr.OF <- true

unit setFP_Underflow() =
    fcsr.OF <- true

unit setFP_Inexact() =
    fcsr.OF <- true

---------------------------------------------------------------------------
-- CSR Register address map
---------------------------------------------------------------------------

-- CSR access control

type csrRW    = bits(2)         -- read/write check
type csrPR    = bits(2)         -- privilege check

csrRW csrRW(csr::csreg)  = csr<11:10>
csrPR csrPR(csr::csreg)  = csr<9:8>

-- this only checks register-level access.  some registers have
-- additional bit-specific read/write controls.
bool check_CSR_access(rw::csrRW, pr::csrPR, p::Privilege, a::accessType) =
    (a == Read or rw != 0b11) and (privLevel(p) >=+ pr)

-- TODO
bool check_hpmcounter_access(ncounter::csreg, p::Privilege)
     = false

bool is_CSR_defined(csr::csreg, p::Privilege) =
    -- user-mode
    csr == 0x000  or  csr == 0x004 or csr == 0x005    -- trap setup
 or (csr >= 0x001 and csr <= 0x003 and haveFP())      -- FP
 or (csr >= 0x040 and csr <= 0x044)                   -- trap handling

 -- basic and hpm counters, for all privileges
 or (csr >= 0xC00 and csr <= 0xC1F and check_hpmcounter_access(csr - 0xC00, p))
 -- RV32I, upper counter bits
 or (csr >= 0xC80 and csr <= 0xC9F and check_hpmcounter_access(csr - 0xC80, p)
     and in32BitMode())

    -- supervisor-mode
 or (csr >= 0x100 and csr <= 0x105 and csr != 0x101)  -- trap setup
 or (csr >= 0x140 and csr <= 0x144)                   -- trap handling
 or (csr == 0x180)                                    -- address translation

    -- machine-mode
 or (csr >= 0xF11 and csr <= 0xF14)                   -- info
 or (csr >= 0x300 and csr <= 0x306)                   -- trap setup
 or (csr >= 0x340 and csr <= 0x344)                   -- trap handling
 -- or (csr >= 0x3A0 and csr <= 0x3A3 and in32BitMode()) -- memory protection configuration
 -- or (csr == 0x3A0 or  csr == 0x3A2)
 -- or (csr >= 0x3B0 and csr <= 0x3BF)                   -- memory protection addresses
 -- or (csr >= 0xB00 and csr <= 0xB1F)                   -- counters
 -- or (csr >= 0xB80 and csr <= 0xB9F and in32BitMode())
 -- or (csr >= 0x323 and csr <= 0x33F)                   -- counter setup

bool check_TVM_SATP(csr::imm12, p::Privilege, a::accessType) =
    not (MCSR.mstatus.M_TVM and p == Supervisor and csr == 0x180)

-- CSR conversion helpers

uip uip_of_mip(i::id) =
{ sip = lower_mip(c_MCSR(i).mip, c_MCSR(i).mideleg)
; lower_sip(sip, c_SCSR(i).sideleg)
}

uie uie_of_mie(i::id) =
{ sie = lower_mie(c_MCSR(i).mie, c_MCSR(i).mideleg)
; lower_sie(sie, c_SCSR(i).sideleg)
}

mstatus mstatus_of_ustatus_64(i::id, v::regType) =
{ m   = c_MCSR(i).mstatus
; s   = lower_mstatus(m)
; s   = legalize_ustatus_64(s, v)
; lift_sstatus(m, s)
}

mstatus mstatus_of_ustatus_32(i::id, v::word) =
{ m   = c_MCSR(i).mstatus
; s   = lower_mstatus(m)
; s   = legalize_ustatus_32(s, v)
; lift_sstatus(m, s)
}

mip mip_of_uip_64(i::id, v::regType) =
{ m   = c_MCSR(i).mip
; md  = c_MCSR(i).mideleg
; sd  = c_SCSR(i).sideleg
; s   = lower_mip(m, md)
; s   = legalize_uip_64(s, sd, v)
; lift_sip(m, s, md)
}

mip mip_of_uip_32(i::id, v::word) =
{ m   = c_MCSR(i).mip
; md  = c_MCSR(i).mideleg
; sd  = c_SCSR(i).sideleg
; s   = lower_mip(m, md)
; s   = legalize_uip_32(s, sd, v)
; lift_sip(m, s, md)
}

mie mie_of_uie_64(i::id, v::regType) =
{ m   = c_MCSR(i).mie
; md  = c_MCSR(i).mideleg
; sd  = c_SCSR(i).sideleg
; s   = lower_mie(m, md)
; s   = legalize_uie_64(s, sd, v)
; lift_sie(m, s, md)
}

mie mie_of_uie_32(i::id, v::word) =
{ m   = c_MCSR(i).mie
; md  = c_MCSR(i).mideleg
; sd  = c_SCSR(i).sideleg
; s   = lower_mie(m, md)
; s   = legalize_uie_32(s, sd, v)
; lift_sie(m, s, md)
}


component CSRMap(csr::csreg) :: regType
{ value =
  -- Implementation note: in 32-bit mode, 32-bit CSR values are
  -- sign-extended so that the sign-bit is preserved.
      match csr, in32BitMode()
      { -- user trap setup
        case 0x000, false   => &lower_sstatus(lower_mstatus(c_MCSR(procID).mstatus))
        case 0x000, true    => ZeroExtend(status_to_32(&lower_sstatus(lower_mstatus(c_MCSR(procID).mstatus))))
        case 0x004, false   => &uie_of_mie(procID)
        case 0x004, true    => ZeroExtend(ipe_to_32(&uie_of_mie(procID)))
        case 0x005, false   => c_UCSR(procID).&utvec
        case 0x005, true    => ZeroExtend(tvec_to_32(c_UCSR(procID).&utvec))

        -- user trap handling
        case 0x040, _       => c_UCSR(procID).uscratch
        case 0x041, _       => c_UCSR(procID).uepc
        case 0x042, false   => c_UCSR(procID).&ucause
        case 0x042, true    => ZeroExtend(cause_to_32(c_UCSR(procID).&ucause))
        case 0x043, _       => c_UCSR(procID).utval
        case 0x044, false   => &uip_of_mip(procID)
        case 0x044, true    => ZeroExtend(ipe_to_32(&uip_of_mip(procID)))

        -- user floating-point context
        case 0x001, _       => ZeroExtend(c_UCSR(procID).&fpcsr<4:0>)
        case 0x002, _       => ZeroExtend(c_UCSR(procID).fpcsr.FRM)
        case 0x003, _       => ZeroExtend(c_UCSR(procID).&fpcsr<7:0>)

        -- counter/timers
        case 0xC00, _       =>            c_MCSR(procID).mcycle
        case 0xC01, _       =>            c_MCSR(procID).mtime
        case 0xC02, _       =>            c_MCSR(procID).minstret
        -- TODO: other hpm counters
        case 0xC80, true    => ZeroExtend(c_MCSR(procID).mcycle<63:32>)
        case 0xC81, true    => ZeroExtend(c_MCSR(procID).mtime<63:32>)
        case 0xC82, true    => ZeroExtend(c_MCSR(procID).minstret<63:32>)
        -- TODO: other hpm counters

        -- supervisor trap setup
        case 0x100, false   => &lower_mstatus(c_MCSR(procID).mstatus)
        case 0x100, true    => ZeroExtend(status_to_32(&lower_mstatus(c_MCSR(procID).mstatus)))
        case 0x102, _       => c_SCSR(procID).&sedeleg
        case 0x103, _       => c_SCSR(procID).&sideleg
        case 0x104, false   => &lower_mie(c_MCSR(procID).mie, c_MCSR(procID).mideleg)
        case 0x104, true    => ZeroExtend(ipe_to_32(&lower_mie(c_MCSR(procID).mie, c_MCSR(procID).mideleg)))
        case 0x105, false   => c_SCSR(procID).&stvec
        case 0x105, true    => ZeroExtend(tvec_to_32(c_SCSR(procID).&stvec))
        case 0x106, _       => ZeroExtend(c_SCSR(procID).&scounteren) -- TODO: check extension

        -- supervisor trap handling
        case 0x140, _       => c_SCSR(procID).sscratch
        case 0x141, _       => c_SCSR(procID).sepc
        case 0x142, false   => c_SCSR(procID).&scause
        case 0x142, true    => ZeroExtend(cause_to_32(c_SCSR(procID).&scause))
        case 0x143, _       => c_SCSR(procID).stval
        case 0x144, false   => &lower_mip(c_MCSR(procID).mip, c_MCSR(procID).mideleg)
        case 0x144, true    => ZeroExtend(ipe_to_32(&lower_mip(c_MCSR(procID).mip, c_MCSR(procID).mideleg)))

        -- supervisor protection and translation
        case 0x180, _       => c_SCSR(procID).satp

        -- machine information registers
        case 0xF11, _       => c_MCSR(procID).mvendorid
        case 0xF12, _       => c_MCSR(procID).marchid
        case 0xF13, _       => c_MCSR(procID).mimpid
        case 0xF14, _       => c_MCSR(procID).mhartid

        -- machine trap setup
        case 0x300, false   => c_MCSR(procID).&mstatus
        case 0x300, true    => ZeroExtend(status_to_32(c_MCSR(procID).&mstatus))
        case 0x301, false   => c_MCSR(procID).&misa
        case 0x301, true    => ZeroExtend(isa_to_32(c_MCSR(procID).&misa))
        case 0x302, _       => c_MCSR(procID).&medeleg
        case 0x303, _       => c_MCSR(procID).&mideleg
        case 0x304, false   => c_MCSR(procID).&mie
        case 0x304, true    => ZeroExtend(ipe_to_32(c_MCSR(procID).&mie))
        case 0x305, false   => c_MCSR(procID).&mtvec
        case 0x305, true    => ZeroExtend(tvec_to_32(c_MCSR(procID).&mtvec))
        case 0x306, _       => ZeroExtend(c_MCSR(procID).&mcounteren)

        -- machine trap handling
        case 0x340, _       => c_MCSR(procID).mscratch
        case 0x341, _       => c_MCSR(procID).mepc
        case 0x342, false   => c_MCSR(procID).&mcause
        case 0x342, true    => ZeroExtend(cause_to_32(c_MCSR(procID).&mcause))
        case 0x343, _       => c_MCSR(procID).mtval
        case 0x344, false   => c_MCSR(procID).&mip
        case 0x344, true    => ZeroExtend(ipe_to_32(c_MCSR(procID).&mip))

        -- machine protection and translation
        -- TODO

        case _              => #UNDEFINED("unexpected CSR read at " : [csr])
      }

  assign value =
      match csr, in32BitMode()
      { -- user trap setup
        case 0x000, false   => c_MCSR(procID).mstatus   <- mstatus_of_ustatus_64(procID, value)
        case 0x000, true    => c_MCSR(procID).mstatus   <- mstatus_of_ustatus_32(procID, value<31:0>)
        case 0x004, false   => c_MCSR(procID).mie       <- mie_of_uie_64(procID, value)
        case 0x004, true    => c_MCSR(procID).mie       <- mie_of_uie_32(procID, value<31:0>)
        case 0x005, false   => c_UCSR(procID).utvec     <- legalize_tvec_64(c_UCSR(procID).utvec, value)
        case 0x005, true    => c_UCSR(procID).utvec     <- legalize_tvec_32(c_UCSR(procID).utvec, value<31:0>)

        -- user trap handling
        case 0x040, _       => c_UCSR(procID).uscratch  <- value
        case 0x041, _       => c_UCSR(procID).uepc      <- (value && SignExtend(0b100`3)) -- no 16-bit instr support
        case 0x042, false   => c_UCSR(procID).&ucause   <- value
        case 0x042, true    => c_UCSR(procID).&ucause   <- cause_of_32(value<31:0>)
        case 0x043, _       => c_UCSR(procID).utval     <- value
        case 0x044, false   => c_MCSR(procID).mip       <- mip_of_uip_64(procID, value)
        case 0x044, true    => c_MCSR(procID).mip       <- mip_of_uip_32(procID, value<31:0>)

        -- user floating-point context
        case 0x001, _       => { c_UCSR(procID).&fpcsr<4:0>     <- value<4:0>
                               ; c_MCSR(procID).mstatus.M_FS    <- ext_status(Dirty)
                               ; c_MCSR(procID).mstatus.M_SD    <- true
                               }
        case 0x002, _       => { c_UCSR(procID).fpcsr.FRM       <- value<2:0>
                               ; c_MCSR(procID).mstatus.M_FS    <- ext_status(Dirty)
                               ; c_MCSR(procID).mstatus.M_SD    <- true
                               }
        case 0x003, _       => { c_UCSR(procID).&fpcsr          <- value<31:0>
                               ; c_MCSR(procID).mstatus.M_FS    <- ext_status(Dirty)
                               ; c_MCSR(procID).mstatus.M_SD    <- true
                               }

        -- user counters/timers are URO

        -- supervisor trap setup
        case 0x100, false   => c_MCSR(procID).mstatus   <- legalize_sstatus_64(c_MCSR(procID).mstatus, value)
        case 0x100, true    => c_MCSR(procID).mstatus   <- legalize_sstatus_32(c_MCSR(procID).mstatus, value<31:0>)
        case 0x102, false   => c_SCSR(procID).sedeleg   <- legalize_sedeleg_64(c_SCSR(procID).sedeleg, value)
        case 0x102, true    => c_SCSR(procID).sedeleg   <- legalize_sedeleg_32(c_SCSR(procID).sedeleg, value<31:0>)
        case 0x103, false   => c_SCSR(procID).sideleg   <- legalize_sideleg_64(c_SCSR(procID).sideleg, value)
        case 0x103, true    => c_SCSR(procID).sideleg   <- legalize_sideleg_32(c_SCSR(procID).sideleg, value<31:0>)
        case 0x104, false   => c_MCSR(procID).mie       <- legalize_sie_64(c_MCSR(procID).mie, c_MCSR(procID).mideleg, value)
        case 0x104, true    => c_MCSR(procID).mie       <- legalize_sie_32(c_MCSR(procID).mie, c_MCSR(procID).mideleg, value<31:0>)
        case 0x105, false   => c_SCSR(procID).stvec     <- legalize_tvec_64(c_SCSR(procID).stvec, value)
        case 0x105, true    => c_SCSR(procID).stvec     <- legalize_tvec_32(c_SCSR(procID).stvec, value<31:0>)

        -- supervisor trap handling
        case 0x140, _       => c_SCSR(procID).sscratch  <- value
        case 0x141, _       => c_SCSR(procID).sepc      <- (value && SignExtend(0b100`3)) -- no 16-bit instr support
        case 0x142, false   => c_SCSR(procID).&scause   <- value
        case 0x142, true    => c_SCSR(procID).&scause   <- cause_of_32(value<31:0>)
        case 0x143, _       => c_SCSR(procID).stval     <- value
        case 0x144, false   => c_MCSR(procID).mip       <- legalize_sip_64(c_MCSR(procID).mip, c_MCSR(procID).mideleg, value)
        case 0x144, true    => c_MCSR(procID).mip       <- legalize_sip_32(c_MCSR(procID).mip, c_MCSR(procID).mideleg, value<31:0>)

        -- supervisor protection and translation
        case 0x180, false   => c_SCSR(procID).satp      <- &satp64(value)
        case 0x180, true    => c_SCSR(procID).satp      <- SignExtend(&satp32(value<31:0>))

        -- supervisor counters/timers are SRO

        -- machine information registers are MRO

        -- machine trap setup
        --  these assignments are done with machine-level privilege.
        case 0x300, false   => c_MCSR(procID).mstatus   <- legalize_mstatus_64(c_MCSR(procID).mstatus, value)
        case 0x300, true    => c_MCSR(procID).mstatus   <- legalize_mstatus_32(c_MCSR(procID).mstatus, value<31:0>)
        case 0x302, false   => c_MCSR(procID).medeleg   <- legalize_medeleg_64(c_MCSR(procID).medeleg, value)
        case 0x302, true    => c_MCSR(procID).medeleg   <- legalize_medeleg_32(c_MCSR(procID).medeleg, value<31:0>)
        case 0x303, false   => c_MCSR(procID).mideleg   <- legalize_mideleg_64(c_MCSR(procID).mideleg, value)
        case 0x303, true    => c_MCSR(procID).mideleg   <- legalize_mideleg_32(c_MCSR(procID).mideleg, value<31:0>)
        case 0x304, false   => c_MCSR(procID).mie       <- legalize_mie_64(c_MCSR(procID).mie, value)
        case 0x304, true    => c_MCSR(procID).mie       <- legalize_mie_32(c_MCSR(procID).mie, value<31:0>)
        case 0x305, false   => c_MCSR(procID).mtvec     <- legalize_tvec_64(c_MCSR(procID).mtvec, value)
        case 0x305, true    => c_MCSR(procID).mtvec     <- legalize_tvec_32(c_MCSR(procID).mtvec, value<31:0>)

        -- machine trap handling
        case 0x340, _       => c_MCSR(procID).mscratch  <- value
        case 0x341, _       => c_MCSR(procID).mepc      <- (value && SignExtend(0b100`3))  -- no 16-bit instr support
        case 0x342, false   => c_MCSR(procID).&mcause   <- value
        case 0x342, true    => c_MCSR(procID).&mcause   <- cause_of_32(value<31:0>)
        case 0x343, _       => c_MCSR(procID).mtval     <- value
        case 0x344, false   => c_MCSR(procID).mip       <- legalize_mip_64(c_MCSR(procID).mip, value)
        case 0x344, true    => c_MCSR(procID).mip       <- legalize_mip_32(c_MCSR(procID).mip, value<31:0>)

        -- machine protection and translation
        -- TODO

        -- machine counters/timers are MRO

        -- machine counter-enables
        -- TODO

        case _, _           => #INTERNAL_ERROR("unexpected CSR write to " : [csr])
      }
}

string csrName(csr::csreg) =
    match csr
    { -- user trap setup
      case 0x000  => "ustatus"
      case 0x004  => "uie"
      case 0x005  => "utvec"

      -- user floating-point context
      case 0x001  => "fflags"
      case 0x002  => "frm"
      case 0x003  => "fcsr"

      -- counter/timers
      case 0xC00  => "cycle"
      case 0xC01  => "time"
      case 0xC02  => "instret"
      case 0xC80  => "cycleh"
      case 0xC81  => "timeh"
      case 0xC82  => "instreth"
      -- TODO: other hpm counters

      -- supervisor trap setup
      case 0x100  => "sstatus"
      case 0x102  => "sedeleg"
      case 0x103  => "sideleg"
      case 0x104  => "sie"
      case 0x105  => "stvec"
      case 0x106  => "scounteren"

      -- supervisor trap handling
      case 0x140  => "sscratch"
      case 0x141  => "sepc"
      case 0x142  => "scause"
      case 0x143  => "stval"
      case 0x144  => "sip"

      -- supervisor protection and translation
      case 0x180  => "satp"

      -- machine information registers
      case 0xF11  => "mvendorid"
      case 0xF12  => "marchid"
      case 0xF13  => "mimpid"
      case 0xF14  => "mhartid"

      -- machine trap setup
      case 0x300  => "mstatus"
      case 0x301  => "misa"
      case 0x302  => "medeleg"
      case 0x303  => "mideleg"
      case 0x304  => "mie"
      case 0x305  => "mtvec"
      case 0x306  => "mcounteren"

      -- machine trap handling
      case 0x340  => "mscratch"
      case 0x341  => "mepc"
      case 0x342  => "mcause"
      case 0x343  => "mtval"
      case 0x344  => "mip"

      -- machine protection and translation
      -- TODO

      -- machine counters/timers
      case 0xB00  => "mcycle"
      case 0xB02  => "minstret"
      case 0xB80  => "mcycleh"
      case 0xB82  => "minstreth"
      -- TODO: other hpm counters and events

      case _      => "UNKNOWN"
    }

---------------------------------------------------------------------------
-- Tandem verification
---------------------------------------------------------------------------
-- The execution side-effects for each instruction are captured in a
-- record, to be used to verify or verify against another
-- implementation.  The values stored here are the result of
-- processing at the current PC.  If any interrupts are pending, those
-- are processed before the PC is fetched.  The StateDelta records the
-- effects of these actions.
--
-- On startup, there is a cross-check of the initial state; however,
-- that is done externally (see model.sml).  The record below only
-- records side-effects once the model actually executes a step.

record StateDelta
{ -- execution context
  priv          :: Privilege    -- the resulting privilege level

  pc            :: regType      -- the next PC

  instr         :: word         -- the fetched instruction, if any, at the current PC

  -- memory addresses and side-effects

  mem_addr      :: regType option   -- any address value computed for
                                    -- the instruction (including
                                    -- loads/stores and control transfer)

  mem_access    :: accessType option    -- type of access
                                    -- loads are Read, stores are Write,
                                    -- AMOs are ReadWrite, control transfer is Execute

                                -- any value accessed at that address
  mem_val       :: (regType * memWidth) option


  -- GPR side-effects
                                -- the register and the value written
  reg_effect    :: (reg * regType) option

  -- FPR side-effects
                                -- the register and the value written
  freg_effect   :: (reg * fpval * fpType) option
                                -- TODO: fpcsr

  -- CSR side-effects (non-exceptional execution)

                                -- the CSR address and the value written
  csr_effect    :: (csreg * regType) option

  -- Exceptions

  exc_taken     :: bool         -- whether an exception (interrupt/trap) was taken
  fetch_exc     :: bool         -- whether that exception occured on fetch
                                --   if so, the instruction (instr) is undefined

                                -- CSRs after the exception has been registered
  mepc          :: regType option
  mcause        :: regType option
  mtval         :: regType option

  sepc          :: regType option
  scause        :: regType option
  stval         :: regType option

  -- mstatus can be updated for many reasons (e.g. writing FPR
  -- can modify M_FS and M_SD), so it is always tracked.
  mstatus       :: regType
}

declare c_update :: id -> StateDelta

component Delta :: StateDelta
{ value        = c_update(procID)
  assign value = c_update(procID) <- value
}

unit initDelta() =
{ Delta.priv        <- User
; Delta.pc          <- ZeroExtend(0b0`1)
; Delta.instr       <- ZeroExtend(0b0`1)

; Delta.mem_addr    <- None
; Delta.mem_access  <- None
; Delta.mem_val     <- None

; Delta.reg_effect  <- None
; Delta.freg_effect <- None
; Delta.csr_effect  <- None

; Delta.exc_taken   <- false
; Delta.fetch_exc   <- false

; Delta.mepc        <- None
; Delta.mcause      <- None
; Delta.mtval       <- None

; Delta.sepc        <- None
; Delta.scause      <- None
; Delta.stval       <- None

-- mstatus is explicitly always recorded.
}

unit recordPC(pc::regType, p::Privilege) =
{ Delta.priv        <- p
; Delta.pc          <- pc
}

unit recordMStatus(ms::mstatus) =
{ Delta.mstatus     <- &ms }

unit recordMCauseEPC(mc::mcause, epc::regType) =
{ Delta.mcause      <- Some(&mc)
; Delta.mepc        <- Some(epc)
}

unit recordMTval(v::regType) =
{ Delta.mtval       <- Some(v) }

unit recordSCauseEPC(sc::mcause, epc::regType) =
{ Delta.scause      <- Some(&sc)
; Delta.sepc        <- Some(epc)
}

unit recordSTval(v::regType) =
{ Delta.stval       <- Some(v) }

unit recordFetch(instr::word) =
{ Delta.instr       <- instr }

unit recordLoad(addr::vAddr, val::regType, width::memWidth) =
{ Delta.mem_addr    <- Some(addr)
; Delta.mem_access  <- Some(Read)
; Delta.mem_val     <- Some(val, width)
}

unit recordStore(addr::vAddr, val::regType, width::memWidth) =
{ Delta.mem_addr    <- Some(addr)
; Delta.mem_access  <- Some(Write)
; Delta.mem_val     <- Some(val, width)
}

unit recordAMOStore(addr::vAddr, val::regType, width::memWidth) =
{ Delta.mem_addr    <- Some(addr)
; Delta.mem_access  <- Some(ReadWrite)
; Delta.mem_val     <- Some(val, width)
}

unit recordException() =
{ Delta.exc_taken   <- true }

unit recordFetchException() =
{ Delta.fetch_exc   <- true }

---------------------------------------------------------------------------
-- Logging
---------------------------------------------------------------------------
string hex12(x::bits(12))   = PadLeft(#"0", 3, [x])
string hex32(x::word)       = PadLeft(#"0", 8, [x])
string hex64(x::dword)      = PadLeft(#"0", 16, [x])

string log_w_csr(csr::csreg, input::regType, final::regType) =
    [ "CSR (0x" : hex12(csr) : ":" : csrName(csr) : ") <- 0x" : hex64(final)
      : " (input: 0x" : hex64(input) : ")" ]

string reg(r::reg) =
{ if      r ==  0 then "$0"
  else if r ==  1 then "ra"
  else if r ==  2 then "sp"
  else if r ==  3 then "gp"
  else if r ==  4 then "tp"
  else if r ==  5 then "t0"
  else if r ==  6 then "t1"
  else if r ==  7 then "t2"
  else if r ==  8 then "fp"
  else if r ==  9 then "s1"
  else if r == 10 then "a0"
  else if r == 11 then "a1"
  else if r == 12 then "a2"
  else if r == 13 then "a3"
  else if r == 14 then "a4"
  else if r == 15 then "a5"
  else if r == 16 then "a6"
  else if r == 17 then "a7"
  else if r == 18 then "s2"
  else if r == 19 then "s3"
  else if r == 20 then "s4"
  else if r == 21 then "s5"
  else if r == 22 then "s6"
  else if r == 23 then "s7"
  else if r == 24 then "s8"
  else if r == 25 then "s9"
  else if r == 26 then "s10"
  else if r == 27 then "s11"
  else if r == 28 then "t3"
  else if r == 29 then "t4"
  else if r == 30 then "t5"
  else                 "t6"
}

string fpreg(r::reg) =
{ if      r ==  0 then "fs0"
  else if r ==  1 then "fs1"
  else if r ==  2 then "fs2"
  else if r ==  3 then "fs3"
  else if r ==  4 then "fs4"
  else if r ==  5 then "fs5"
  else if r ==  6 then "fs6"
  else if r ==  7 then "fs7"
  else if r ==  8 then "fs8"
  else if r ==  9 then "fs9"
  else if r == 10 then "fs10"
  else if r == 11 then "fs11"
  else if r == 12 then "fs12"
  else if r == 13 then "fs13"
  else if r == 14 then "fs14"
  else if r == 15 then "fs15"
  else if r == 16 then "fv0"
  else if r == 17 then "fv1"
  else if r == 18 then "fa0"
  else if r == 19 then "fa1"
  else if r == 20 then "fa2"
  else if r == 21 then "fa3"
  else if r == 22 then "fa4"
  else if r == 23 then "fa5"
  else if r == 24 then "fa6"
  else if r == 25 then "fa7"
  else if r == 26 then "ft0"
  else if r == 27 then "ft1"
  else if r == 28 then "ft2"
  else if r == 29 then "ft3"
  else if r == 30 then "ft4"
  else                 "ft5"
}

string log_w_gpr(r::reg, data::regType) =
    "Reg " : reg(r) : " (" : [[r]::nat] : ") <- 0x" : hex64(data)

string log_w_fprs(r::reg, data::word) =
    "FPR " : reg(r) : " (" : [[r]::nat] : ") <- 0x" : hex32(data)

string log_w_fprd(r::reg, data::regType) =
    "FPR " : reg(r) : " (" : [[r]::nat] : ") <- 0x" : hex64(data)

string log_w_mem_mask(pAddrIdx::pAddrIdx, vAddr::vAddr, mask::regType,
                      data::regType, old::regType, new::regType) =
    "MEM[0x" : hex64([pAddrIdx]) : "/" : hex64(vAddr) :
    "] <- (data: 0x" : hex64(data) : ", mask: 0x" : hex64(mask) :
    ", old: 0x"  : hex64(old) : ", new: 0x"  : hex64(new) : ")"

string log_w_mem_mask_misaligned(pAddrIdx::pAddrIdx, vAddr::vAddr, mask::regType,
                                 data::regType, align::nat, old::regType, new::regType) =
    "MEM[0x" : hex64([pAddrIdx]) : "/" : hex64(vAddr) : "/ misaligned@" : [align] :
    "] <- (data: 0x" : hex64(data) : ", mask: 0x" : hex64(mask) :
    ", old: 0x"  : hex64(old) : ", new: 0x"  : hex64(new) : ")"

string log_w_mem(pAddrIdx::pAddrIdx, vAddr::vAddr, data::regType) =
    "MEM[0x" : hex64([pAddrIdx]) : "/" : hex64(vAddr) :
    "] <- (data: 0x" : hex64(data) : ")"

string log_r_mem(pAddrIdx::pAddrIdx, vAddr::vAddr, data::regType) =
    "data <- MEM[0x" : PadLeft(#"0", 10, [pAddrIdx]) : "/" : hex64(vAddr) :
    "]: 0x" : hex64(data)

string log_exc(e::ExceptionType) =
    " Exception " : [e]::string : " raised!"

string log_tohost(tohost::regType) =
    "-> host: " : hex64(tohost)

nat LOG_IO      = 0
nat LOG_INSN    = 1
nat LOG_REG     = 2
nat LOG_MEM     = 3
nat LOG_ADDRTR  = 4

declare log :: (nat * string) list

unit mark_log(lvl::nat, s::string)  = log <- (lvl, s) @ log
unit clear_logs()                   = log <- Nil

---------------------------------------------------------------------------
-- Exception and Interrupt processing
---------------------------------------------------------------------------

-- Signalled exceptions are recorded as traps.

unit setTrap(e::ExceptionType, badaddr::vAddr option) =
{ var trap
; trap.trap             <- e
; trap.badaddr          <- badaddr
; NextFetch             <- Some(Trap(trap))
}

unit signalException(e::ExceptionType) =
{ mark_log(LOG_INSN, "signalling exception " : [e]::string)
; setTrap(e, None)
; recordException()
}

unit signalAddressException(e::ExceptionType, vAddr::vAddr) =
{ mark_log(LOG_INSN, "signalling address exception " : [e]::string : " at " : [vAddr])
; setTrap(e, Some(vAddr))
; recordException()
}

-- Trap vector address

regType tvec_addr(m::mtvec, c::mcause) =
{ base = [m.BASE : 0b0`2]::regType
; match [m.MODE]::TrapVectorMode
  { case TV_Direct => { base }
    case TV_Vector => { if c.M_Intr
                        then base + (ZeroExtend(c.M_ExcCause)::regType << 2)
                        else base
                      }
    case _         => #INTERNAL_ERROR("Invalid tvec MODE")
  }
}

-- Delegation logic.

Privilege excHandlerDelegate(e::ExceptionType) =
{ eidx  = [e]::nat
; super = MCSR.&medeleg<eidx>
; user  = super and SCSR.&sedeleg<eidx>
; if      MCSR.misa.N and user  then User
  else if MCSR.misa.S and super then Supervisor
  else                               Machine
}

-- Handling logic.

unit excHandler(intr::bool, ec::exc_code, fromPriv::Privilege, toPriv::Privilege,
                epc::regType, badaddr::vAddr option) =
{ mark_log(LOG_INSN, ["trapping from " : privName(fromPriv) : " to " : privName(toPriv) :
                      " at pc " : [epc] : (if intr then " intr:#" else " exc:#") : [[ec]::nat] :
                      [if IsSome(badaddr) then [" tval:" : [ValOf(badaddr)]] else ""]])
; match toPriv
  { case Machine    => { MCSR.mstatus           <- menter(MCSR.mstatus, fromPriv)
                       ; MCSR.mepc              <- epc
                       ; MCSR.mcause.M_Intr     <- intr
                       ; MCSR.mcause.M_ExcCause <- ZeroExtend(ec)
                       ; MCSR.mtval             <- if IsSome(badaddr)
                                                   then ValOf(badaddr)
                                                   else SignExtend(0b0`1)
                       ; PC                     <- tvec_addr(MCSR.mtvec, MCSR.mcause)

                       ; recordMCauseEPC(MCSR.mcause, epc)
                       ; recordMTval(MCSR.mtval)
                       ; recordMStatus(MCSR.mstatus)
                       }
    case Supervisor => { MCSR.mstatus           <- senter(MCSR.mstatus, fromPriv)
                       ; SCSR.sepc              <- epc
                       ; SCSR.scause.M_Intr     <- intr
                       ; SCSR.scause.M_ExcCause <- ZeroExtend(ec)
                       ; SCSR.stval             <- if IsSome(badaddr)
                                                   then ValOf(badaddr)
                                                   else SignExtend(0b0`1)

                       ; PC                     <- tvec_addr(SCSR.stvec, SCSR.scause)

                       ; recordSCauseEPC(SCSR.scause, epc)
                       ; recordSTval(SCSR.stval)
                       ; recordMStatus(MCSR.mstatus)
                       }
    case User       => { MCSR.mstatus           <- uenter(MCSR.mstatus, fromPriv)
                       ; UCSR.uepc              <- epc
                       ; UCSR.ucause.M_Intr     <- intr
                       ; UCSR.ucause.M_ExcCause <- ZeroExtend(ec)
                       ; UCSR.utval             <- if IsSome(badaddr)
                                                   then ValOf(badaddr)
                                                   else SignExtend(0b0`1)
                       ; PC                     <- tvec_addr(UCSR.utvec, UCSR.ucause)

                       ; recordMStatus(MCSR.mstatus)
                       }
  }
; curPrivilege <- toPriv
}

-- Interrupts are prioritized in privilege order, and for each
-- privilege, in the order: external, software, timers.

InterruptType option searchDispatchableIntr(ip::mip) =
{ intr = I_M_External
; idx  = [interruptIndex(intr)]::nat
; if (&ip)<idx> then Some(intr)
  else {
  intr = I_M_Software
; idx  = [interruptIndex(intr)]::nat
; if (&ip)<idx> then Some(intr)
  else {
  intr = I_M_Timer
; idx  = [interruptIndex(intr)]::nat
; if (&ip)<idx> then Some(intr)
  else {
  intr = I_S_External
; idx  = [interruptIndex(intr)]::nat
; if (&ip)<idx> then Some(intr)
  else {
  intr = I_S_Software
; idx  = [interruptIndex(intr)]::nat
; if (&ip)<idx> then Some(intr)
  else {
  intr = I_S_Timer
; idx  = [interruptIndex(intr)]::nat
; if (&ip)<idx> then Some(intr)
  else None
  }}}}}
}

(InterruptType * Privilege) option curInterrupt() =
{ e_mip = MCSR.&mip && MCSR.&mie
; if   e_mip == 0 or not MCSR.mstatus.M_MIE then None -- fast path
  else {
  -- This assumes 'S' mode and no 'N' extension.  It will need
  -- revision for other configurations (e.g. M/U).
    m_ip = e_mip && ~MCSR.&mideleg
  ; s_ip = e_mip &&  MCSR.&mideleg

  -- dispatch in order of decreasing privilege
  ; if      m_ip != 0 and MCSR.mstatus.M_MIE
    then    match searchDispatchableIntr(mip(m_ip))
         { case Some(i) => Some(i, Machine)
           case None    => None
         }
    else if s_ip != 0 and MCSR.mstatus.M_SIE
    then    match searchDispatchableIntr(mip(s_ip))
         { case Some(i) => Some(i, Supervisor)
           case None    => None
         }
    else    None
  }
}


---------------------------------------------------------------------------
-- CSR access with logging
---------------------------------------------------------------------------

component CSR(n::csreg) :: regType
{ value        = CSRMap(n)
  assign value =  { CSRMap(n) <- value
                  ; mark_log(LOG_REG, log_w_csr(n, value, CSRMap(n)))
                  }
}

unit writeCSR(csr::csreg, val::regType) =
{ CSR(csr)          <- val;
  -- Note that writes to CSR are intercepted and controlled by CSRMap,
  -- so we need to use what was finally written to the CSR, and not
  -- val itself.
  Delta.csr_effect  <- Some(csr, CSR(csr))
}

---------------------------------------------------------------------------
-- GPR/FPR access with logging
---------------------------------------------------------------------------

component GPR(n::reg) :: regType
{ value        = if n == 0 then 0 else gpr(n)
  assign value = when n <> 0
                 do { gpr(n) <- value
                    ; mark_log(LOG_REG, log_w_gpr(n, value))
                    }
}

unit writeRD(rd::reg, val::regType) =
{ GPR(rd)           <- val
; Delta.reg_effect  <- Some(rd, if rd == 0 then 0 else val)
}

component FPRS(n::reg) :: word
{ value        = fpr(n)<31:0>
  assign value = { fpr(n)<31:0> <- value
                 ; mark_log(LOG_REG, log_w_fprs(n, value))
                 }
}

component FPRD(n::reg) :: regType
{ value        = fpr(n)
  assign value = { fpr(n) <- value
                 ; mark_log(LOG_REG, log_w_fprd(n, value))
                 }
}

unit writeFPRS(rd::reg, val::word) =
{ FPRS(rd)          <- val
; MCSR.mstatus.M_FS <- ext_status(Dirty)
; MCSR.mstatus.M_SD <- true
; Delta.freg_effect <- Some(rd, ZeroExtend(val), FP_single)
}

unit writeFPRD(rd::reg, val::regType) =
{ FPRD(rd)          <- val
; MCSR.mstatus.M_FS <- ext_status(Dirty)
; MCSR.mstatus.M_SD <- true
; Delta.freg_effect <- Some(rd, val, FP_double)
}

---------------------------------------------------------------------------
-- Raw memory access
---------------------------------------------------------------------------

declare MEM :: pAddrIdx -> regType -- raw memory, laid out in blocks
                                   -- of (|pAddr|-|pAddrIdx|) bits

-- Spike HTIF compatibility
-- The riscv-test suite uses the tohost MMIO port to indicate test completion
-- and pass/fail status.
declare htif_tohost_addr :: pAddr  -- address of tohost port
declare done :: bool               -- internal flag to request termination

unit initMem(val::regType) =
    MEM <- InitMap(val)

regType rawReadData(pAddr::pAddr) =
{ pAddrIdx = pAddr<63:3>
; align    = [pAddr<2:0>]::nat
; if align == 0   -- aligned read
  then { data = MEM(pAddrIdx)
       ; mark_log(LOG_MEM, log_r_mem(pAddrIdx,   pAddr, data))
       ; data
       }
  else { dw0   = MEM(pAddrIdx)
       ; dw1   = MEM(pAddrIdx+1)
       ; ddw   = (dw1 : dw0) >> (align * 8)
       ; data  = ddw<63:0>
       ; mark_log(LOG_MEM, log_r_mem(pAddrIdx,   pAddr, dw0))
       ; mark_log(LOG_MEM, log_r_mem(pAddrIdx+1, pAddr, dw1))
       ; mark_log(LOG_MEM, log_r_mem(pAddrIdx,   pAddr, data))
       ; data
       }
}

unit rawWriteData(pAddr::pAddr, data::regType, nbytes::nat) =
{ mask     = ([ZeroExtend(1`1)::regType] << (nbytes * 8)) - 1
; pAddrIdx = pAddr<63:3>
; align    = [pAddr<2:0>] :: nat
; old      = MEM(pAddrIdx)

; mark_log(LOG_MEM, log_r_mem(pAddrIdx, pAddr, old))

; if align == 0     -- aligned write
  then { new = old && ~mask || data && mask
       ; MEM(pAddrIdx) <- new
       ; mark_log(LOG_MEM, log_w_mem_mask(pAddrIdx, pAddr, mask, data, old, new))
       }
  else { if align + nbytes <= Size(mask) div 8      -- write to a single regType-sized block
         then { new = old && ~(mask << (align * 8)) || (data && mask) << (align * 8)
              ; MEM(pAddrIdx) <- new
              ; mark_log(LOG_MEM, log_w_mem_mask_misaligned(pAddrIdx, pAddr, mask, data, align, old, new))
              }
         -- write touching adjacent regType-sized blocks
         else { dw_old  = MEM(pAddrIdx+1) : old
              ; dw_data = ZeroExtend(data) << (align*8)
              ; dw_mask = ZeroExtend(mask) << (align*8)
              ; dw_new  = dw_old && ~dw_mask || dw_data && dw_mask
              ; MEM(pAddrIdx+1) <- dw_new<2*Size(data)-1:Size(data)>
              ; MEM(pAddrIdx)   <- dw_new<Size(data)-1:0>
              }
       }
; if pAddr == htif_tohost_addr
  then { mark_log(LOG_MEM, log_tohost(data))
       ; ExitCode <- data
       ; done     <- true
       }
  else ()
}

word rawReadInst(pAddr::pAddr) =
{ pAddrIdx = pAddr<63:3>
; data     = MEM(pAddrIdx)
; mark_log(LOG_MEM, log_r_mem(pAddrIdx, pAddr, data))
; if pAddr<2> then data<63:32> else data<31:0>
}

-- helper used to preload memory contents
unit rawWriteMem(pAddr::pAddr, data::regType) =
{ pAddrIdx = pAddr<63:3>
; MEM(pAddrIdx) <- data
; mark_log(LOG_MEM, log_w_mem(pAddrIdx, pAddr, data))
}

---------------------------------------------------------------------------
-- Address Translation
---------------------------------------------------------------------------

-- implementation options

declare enable_dirty_update :: bool
-- if true, the page-table walker updates PTEs when needed.
-- if false, a page-fault exception is thrown instead.

nat PAGESIZE_BITS     = 12

-- internal defines for TLB implementation
nat  TLBEntries       = 16
type tlbIdx           = bits(4)

-- PTE attributes

type pteAttribs = bits(8)

register pteBits :: pteAttribs
{ 7 : PTE_D
, 6 : PTE_A
, 5 : PTE_G
, 4 : PTE_U
, 3 : PTE_X
, 2 : PTE_W
, 1 : PTE_R
, 0 : PTE_V
}

bool isPTEPtr(p::pteAttribs)    = p<3:1> == 0
bool isInvalidPTE(p::pteBits)   = (not p.PTE_V) or (p.PTE_W and not p.PTE_R)

bool checkPTEPermission(ac::accessType, priv::Privilege, mxr::bool, sum::bool, p::pteBits) =
{ match ac, priv
  { case Read,      User        => p.PTE_U            and (p.PTE_R or (mxr and p.PTE_X))
    case Write,     User        => p.PTE_U            and p.PTE_W
    case ReadWrite, User        => p.PTE_U            and (p.PTE_R or (mxr and p.PTE_X)) and p.PTE_W
    case Execute,   User        => p.PTE_U            and p.PTE_X

    case Read,      Supervisor  => (!p.PTE_U or sum)  and (p.PTE_R or (mxr and p.PTE_X))
    case Write,     Supervisor  => (!p.PTE_U or sum)  and p.PTE_W
    case ReadWrite, Supervisor  => (!p.PTE_U or sum)  and (p.PTE_R or (mxr and p.PTE_X)) and p.PTE_W
    case Execute,   Supervisor  => !p.PTE_U           and p.PTE_X

    case _,         Machine     => #INTERNAL_ERROR("machine mem perm check")    -- should not happen
  }
}

-- returns an updated set of PTE bits for an access, if needed
pteBits option updatePTEBits(p::pteBits, ac::accessType) =
{ d_update = (ac == Write or ac == ReadWrite) and (not p.PTE_D)
; a_update = not p.PTE_A
; if d_update or a_update
  then { var p_w = p
       ; p_w.PTE_A <- true
       ; when d_update do p_w.PTE_D <- true
       ; Some(p_w)
       }
  else None
}


-- Sv32 memory translation
--------------------------

nat SV32_LEVEL_BITS = 10
nat PTE32_LOG_SIZE  =  2
nat SV32_LEVELS     =  2

type vaddr32  = bits(32)
type paddr32  = bits(34)
type pte32    = word

register SV32_Vaddr :: vaddr32
{ 31-12 : VA_VPNi   -- VPN[1,0]
, 11-0  : VA_PgOfs  -- page offset
}

register SV32_Paddr :: paddr32
{ 33-12 : PA_PPNi   -- PPN[1,0]
, 11-0  : PA_PgOfs  -- page offset
}

register SV32_PTE   :: pte32
{ 31-10 : PTE_PPNi  -- PPN[1,0]
,   9-8 : PTE_RSW   -- reserved for supervisor software
,   7-0 : PTE_BITS  -- attributes
}

paddr32 curPTB32() =
    (ZeroExtend(satp32(SCSR.satp<31:0>).SATP32_PPN) << PAGESIZE_BITS)

-- 32-bit page table walker.
---------------------------------------------------------------------------
-- This returns the physical address for the input vaddr32 as the
-- first element of the returned tuple.  The remaining elements are
-- for the TLB implementation: the PTE entry itself, the address of
-- the PTE in memory (so that it can be updated) by the TLB, the level
-- of the PTE entry, and whether the mapping is marked as a global
-- mapping.

(paddr32 * SV32_PTE * paddr32 * nat * bool) option
walk32(vaddr::vaddr32, ac::accessType, priv::Privilege, mxr::bool, sum::bool,
       ptb::paddr32, level::nat, global::bool) =
{ va        = SV32_Vaddr(vaddr)
; pt_ofs    = ZeroExtend((va.VA_VPNi >>+ (level * SV32_LEVEL_BITS))<(SV32_LEVEL_BITS-1):0>) << PTE32_LOG_SIZE
; pte_addr  = ptb + pt_ofs
; pte       = SV32_PTE(rawReadData(ZeroExtend(pte_addr))<31:0>)
; pbits     = pteBits(pte.PTE_BITS)
; mark_log(LOG_ADDRTR, ["walk32(vaddr=0x" : PadLeft(#"0", 16, [&va]) : "): level=" : [level]
                        : " pt_base=0x" : PadLeft(#"0", 16, [ptb])
                        : " pt_ofs=" : [[pt_ofs]::nat]
                        : " pte_addr=0x" : PadLeft(#"0", 16, [pte_addr])
                        : " pte=0x" : PadLeft(#"0", 16, [&pte])])
; if   isInvalidPTE(pbits)
  then { mark_log(LOG_ADDRTR, "walk32: invalid PTE")
       ; None
       }
  else { if isPTEPtr(&pbits)
         then { -- ptr to next level
                if level == 0
                then { mark_log(LOG_ADDRTR, "last-level PTE contains a pointer!")
                     ; None
                     }
                else walk32(vaddr, ac, priv, mxr, sum,
                            ZeroExtend(pte.PTE_PPNi << PAGESIZE_BITS), level - 1, global or pbits.PTE_G)
              }
         else { -- leaf PTE
                if not checkPTEPermission(ac, priv, mxr, sum, pbits)
                then { mark_log(LOG_ADDRTR, "PTE permission check failure!")
                     ; None
                     }
                else { if   level > 0
                       then { mask = (1 << (level * SV32_LEVEL_BITS)) - 1
                            ; if   pte.PTE_PPNi && mask != ZeroExtend(0b0`1)
                              then { mark_log(LOG_ADDRTR, "misaligned superpage mapping!")
                                   ; None
                                   }
                              else { ppn = pte.PTE_PPNi || (ZeroExtend(va.VA_VPNi) && mask)
                                   ; Some(ppn : va.VA_PgOfs, pte, pte_addr, level, global or pbits.PTE_G)
                                   }
                            }
                       else Some(pte.PTE_PPNi : va.VA_PgOfs, pte, pte_addr, level, global or pbits.PTE_G)
                     }
              }
       }
}


-- 32-bit TLB
---------------------------------------------------------------------------
-- The spec discusses but does not specify a TLB, and we would like to
-- capture part of the semantics of SFENCE.  A TLB also improves
-- simulation speed.

-- This implementation stores the global mapping bit from the PTE in
-- the TLB.  This causes an asymmetry between TLB lookup and TLB
-- flush, due to the spec's treatment of an ASID=0 in SFENCE.VM:
--
-- * in TLB lookup, the global bit is used to check for a global
--   match, and this global bit when set overrides the input ASID.
--
-- * in TLB flush, an input ASID of 0 overrides the global bit when
--   checking if a TLB entry needs to be flushed.

-- Each TLBEntry also stores the full PTE and its pAddr, so that it
-- can write back the PTE when its dirty bit needs to be updated.

record TLB32_Entry
{ asid          :: asid32
  global        :: bool
  vAddr         :: vaddr32      -- VPN
  vMatchMask    :: vaddr32      -- matching mask for superpages

  pAddr         :: paddr32      -- PPN
  vAddrMask     :: vaddr32      -- selection mask for superpages

  pte           :: SV32_PTE     -- for permissions and dirty bit writeback
  pteAddr       :: paddr32

  age           :: regType      -- derived from cycles
}

TLB32_Entry mkTLB32_Entry(asid::asid32, global::bool, vAddr::vaddr32, pAddr::paddr32,
                          pte::SV32_PTE, i::nat, pteAddr::paddr32) =
{ var ent :: TLB32_Entry
; ent.asid          <- asid
; ent.global        <- global
; ent.pte           <- pte
; ent.pteAddr       <- pteAddr
; ent.vAddrMask     <- ((1::vaddr32) << ((SV32_LEVEL_BITS*i) + PAGESIZE_BITS)) - 1
; ent.vMatchMask    <- (SignExtend('1')::vaddr32) ?? ent.vAddrMask
; ent.vAddr         <- vAddr && ent.vMatchMask
; ent.pAddr         <- (pAddr >> (PAGESIZE_BITS + (SV32_LEVEL_BITS*i))) << (PAGESIZE_BITS + (SV32_LEVEL_BITS*i))
; ent.age           <- c_cycles(procID)
; ent
}

type TLB32_Map  = tlbIdx -> TLB32_Entry option

(TLB32_Entry * tlbIdx) option lookupTLB32(asid::asid32, vAddr::vaddr32, tlb::TLB32_Map) =
{ var ent = None
; for i in 0 .. TLBEntries - 1 do
  { match tlb([i])
    { case Some(e) => when ent == None and (e.global or e.asid == asid)
                           and (e.vAddr == vAddr && e.vMatchMask)
                      do ent <- Some(e, [i])
      case None    => ()
    }
  }
; ent
}

TLB32_Map addToTLB32(asid::asid32, vAddr::vaddr32, pAddr::paddr32, pte::SV32_PTE, pteAddr::paddr32,
                     i::nat, global::bool, curTLB::TLB32_Map) =
{ ent           = mkTLB32_Entry(asid, global, vAddr, pAddr, pte, i, pteAddr)
; var tlb       = curTLB
; var current   = SignExtend('1')
; var addIdx    = 0
; var added     = false
; for i in 0 .. TLBEntries - 1 do
  { match tlb([i])
    { case Some(e)  => when e.age   <+ current
                       do { current <- e.age
                          ; addIdx  <- i
                          }
      case None     => { tlb([i])   <- Some(ent)
                       ; added      <- true
                       }
    }
  }
; when not added
  do tlb([addIdx]) <- Some(ent)
; tlb
}

TLB32_Map flushTLB32(asid::asid32, addr::vaddr32 option, curTLB::TLB32_Map) =
{ var tlb = curTLB
; for i in 0 .. TLBEntries - 1 do
  { match tlb([i]), addr
    { case Some(e), Some(va)    => when (asid == 0 or (asid == e.asid and not e.global))
                                        and (e.vAddr == va && e.vMatchMask)
                                   do tlb([i]) <- None
      case Some(e), None        => when asid == 0 or (asid == e.asid and not e.global)
                                   do tlb([i]) <- None
      case None,    _           => ()
    }
  }
; tlb
}

declare  c_tlb32 :: id -> TLB32_Map

component TLB32 :: TLB32_Map
{ value        = c_tlb32(procID)
  assign value = c_tlb32(procID) <- value
}

-- Sv32 address translation

paddr32 option translate32(vAddr::vaddr32, ac::accessType, priv::Privilege, mxr::bool, sum::bool, level::nat) =
{ asid = curAsid32()
; match lookupTLB32(asid, vAddr, TLB32)
  { case Some(ent, idx) =>
    { if checkPTEPermission(ac, priv, mxr, sum, pteBits(ent.pte.PTE_BITS))
      then { mark_log(LOG_ADDRTR, "TLB32 hit!")
           ; match updatePTEBits(pteBits(ent.pte.PTE_BITS), ac)
             { case None =>
                  Some(ent.pAddr || ZeroExtend(vAddr && ent.vAddrMask))
               case Some(pbits) =>
               { if   enable_dirty_update
                 then { var n_ent = ent
                      ; var tlb   = TLB32
                      -- update entry and TLB
                      ; n_ent.pte.PTE_BITS  <- &pbits
                      ; tlb([idx])          <- Some(n_ent)
                      ; TLB32               <- tlb
                      -- update memory
                      ; rawWriteData(ZeroExtend(n_ent.pteAddr), ZeroExtend(n_ent.&pte), 4)
                      -- done
                      ; Some(n_ent.pAddr || ZeroExtend(vAddr && n_ent.vAddrMask))
                      }
                 else { mark_log(LOG_ADDRTR, "tlb/pte needs dirty/accessed update!")
                      ; None
                      }
               }
             }
           }
      else { mark_log(LOG_ADDRTR, "TLB32 permission check failure")
           ; None
           }
    }
    case None =>
    { mark_log(LOG_ADDRTR, "TLB32 miss!")
    ; match walk32(vAddr, ac, priv, mxr, sum, curPTB32(), level, false)
      { case None   =>
          None
        case Some(pAddr, pte, pteAddr, i, global)  =>
        { match updatePTEBits(pteBits(pte.PTE_BITS), ac)
          { case None =>
            { TLB32 <- addToTLB32(asid, vAddr, pAddr, pte, pteAddr, i, global, TLB32)
            ; Some(pAddr)
            }
            case Some(pbits) =>
            { if   enable_dirty_update
              then { var pte_w = pte
                   ; pte_w.PTE_BITS <- &pbits
                   ; rawWriteData(ZeroExtend(pteAddr), ZeroExtend(&pte_w), 4)
                   ; TLB32 <- addToTLB32(asid, vAddr, pAddr, pte_w, pteAddr, i, global, TLB32)
                   ; Some(pAddr)
                   }
              else { mark_log(LOG_ADDRTR, "pte needs dirty/accessed update!")
                   ; None
                   }
            }
          }
        }
      }
    }
  }
}

-- Sv39 memory translation
--------------------------

nat SV39_LEVEL_BITS = 9
nat SV39_LEVELS     = 3
nat PTE39_LOG_SIZE  = 3

type vaddr39  = bits(39)
type paddr39  = bits(56)
type pte39    = dword

register SV39_Vaddr :: vaddr39
{ 38-12 : VA_VPNi   -- VPN[2,0]
, 11-0  : VA_PgOfs  -- page offset
}

register SV39_Paddr :: paddr39
{ 55-12 : PA_PPNi   -- PPN[2,0]
, 11-0  : PA_PgOfs  -- page offset
}

register SV39_PTE   :: pte39
{ 53-10 : PTE_PPNi  -- PPN[2,0]
,   9-8 : PTE_RSW   -- reserved for software use
,   7-0 : PTE_BITS  -- attributes
}

paddr39 curPTB39() =
    (ZeroExtend(satp64(SCSR.satp).SATP64_PPN) << PAGESIZE_BITS)

-- 64-bit page table walker.

(paddr39 * SV39_PTE * paddr39 * nat * bool) option
walk39(vaddr::vaddr39, ac::accessType, priv::Privilege, mxr::bool, sum::bool,
       ptb::paddr39, level::nat, global::bool) =
{ va        = SV39_Vaddr(vaddr)
; pt_ofs    = ZeroExtend((va.VA_VPNi >>+ (level * SV39_LEVEL_BITS))<(SV39_LEVEL_BITS-1):0>) << PTE39_LOG_SIZE
; pte_addr  = ptb + pt_ofs
; pte       = SV39_PTE(rawReadData(ZeroExtend(pte_addr)))
; pbits     = pteBits(pte.PTE_BITS)
; mark_log(LOG_ADDRTR, ["walk39(vaddr=0x" : PadLeft(#"0", 16, [&va]) : "): level=" : [level]
                        : " pt_base=0x" : PadLeft(#"0", 16, [ptb])
                        : " pt_ofs=" : [[pt_ofs]::nat]
                        : " pte_addr=0x" : PadLeft(#"0", 16, [pte_addr])
                        : " pte=0x" : PadLeft(#"0", 16, [&pte])])
; if   isInvalidPTE(pbits)
  then { mark_log(LOG_ADDRTR, "walk39: invalid PTE!")
       ; None
       }
  else { if   isPTEPtr(&pbits)
         then { -- ptr to next level
                if   level == 0
                then { mark_log(LOG_ADDRTR, "last-level PTE contains a pointer!")
                     ; None
                     }
                else walk39(vaddr, ac, priv, mxr, sum,
                            ZeroExtend(pte.PTE_PPNi << PAGESIZE_BITS), level - 1, global or pbits.PTE_G)
              }
         else { -- leaf PTE
                if   not checkPTEPermission(ac, priv, mxr, sum, pbits)
                then { mark_log(LOG_ADDRTR, "PTE permission check failure!")
                     ; None
                     }
                else { if   level > 0
                       then { mask = (1 << (level * SV39_LEVEL_BITS)) - 1
                            ; if   pte.PTE_PPNi && mask != ZeroExtend(0b0`1)
                              then { mark_log(LOG_ADDRTR, "misaligned superpage mapping!")
                                   ; None
                                   }
                              else { ppn = pte.PTE_PPNi || (ZeroExtend(va.VA_VPNi) && mask)
                                   ; Some(ppn : va.VA_PgOfs, pte, pte_addr, level, global or pbits.PTE_G)
                                   }
                            }
                       else Some(pte.PTE_PPNi : va.VA_PgOfs, pte, pte_addr, level, global or pbits.PTE_G)
                     }
              }
       }
}

-- 64-bit TLB

record TLB39_Entry
{ asid          :: asid64
  global        :: bool
  vAddr         :: vaddr39      -- VPN
  vMatchMask    :: vaddr39      -- matching mask for superpages

  pAddr         :: paddr39      -- PPN
  vAddrMask     :: vaddr39      -- selection mask for superpages

  pte           :: SV39_PTE     -- for permissions and dirty bit writeback
  pteAddr       :: paddr39

  age           :: regType      -- derived from cycles
}

TLB39_Entry mkTLB39_Entry(asid::asid64, global::bool, vAddr::vaddr39, pAddr::paddr39,
                          pte::SV39_PTE, i::nat, pteAddr::paddr39) =
{ var ent :: TLB39_Entry
; ent.asid          <- asid
; ent.global        <- global
; ent.pte           <- pte
; ent.pteAddr       <- pteAddr
; ent.vAddrMask     <- ((1::vaddr39) << ((SV39_LEVEL_BITS*i) + PAGESIZE_BITS)) - 1
; ent.vMatchMask    <- (SignExtend('1')::vaddr39) ?? ent.vAddrMask
; ent.vAddr         <- vAddr && ent.vMatchMask
; ent.pAddr         <- (pAddr >> (PAGESIZE_BITS + (SV39_LEVEL_BITS*i))) << (PAGESIZE_BITS + (SV39_LEVEL_BITS*i))
; ent.age           <- c_cycles(procID)
; ent
}

type TLB39_Map  = tlbIdx -> TLB39_Entry option

(TLB39_Entry * tlbIdx) option lookupTLB39(asid::asid64, vAddr::vaddr39, tlb::TLB39_Map) =
{ var ent = None
; for i in 0 .. TLBEntries - 1 do
  { match tlb([i])
    { case Some(e) => when ent == None and (e.global or e.asid == asid)
                           and (e.vAddr == vAddr && e.vMatchMask)
                      do ent <- Some(e, [i])
      case None    => ()
    }
  }
; ent
}

TLB39_Map addToTLB39(asid::asid64, vAddr::vaddr39, pAddr::paddr39, pte::SV39_PTE, pteAddr::paddr39,
                     i::nat, global::bool, curTLB::TLB39_Map) =
{ ent           = mkTLB39_Entry(asid, global, vAddr, pAddr, pte, i, pteAddr)
; var tlb       = curTLB
; var current   = SignExtend('1')
; var addIdx    = 0
; var added     = false
; for i in 0 .. TLBEntries - 1 do
  { match tlb([i])
    { case Some(e)  => when e.age   <+ current
                       do { current <- e.age
                          ; addIdx  <- i
                          }
      case None     => { tlb([i])   <- Some(ent)
                       ; added      <- true
                       }
    }
  }
; when not added
  do tlb([addIdx]) <- Some(ent)
; tlb
}

TLB39_Map flushTLB39(asid::asid64, addr::vaddr39 option, curTLB::TLB39_Map) =
{ var tlb = curTLB
; for i in 0 .. TLBEntries - 1 do
  { match tlb([i]), addr
    { case Some(e), Some(va)    => when (asid == 0 or (asid == e.asid and not e.global))
                                        and (e.vAddr == va && e.vMatchMask)
                                   do tlb([i]) <- None
      case Some(e), None        => when asid == 0 or (asid == e.asid and not e.global)
                                   do tlb([i]) <- None
      case None,    _           => ()
    }
  }
; tlb
}

declare  c_tlb39 :: id -> TLB39_Map

component TLB39 :: TLB39_Map
{ value        = c_tlb39(procID)
  assign value = c_tlb39(procID) <- value
}

-- Sv39 address translation

paddr39 option translate39(vAddr::vaddr39, ac::accessType, priv::Privilege, mxr::bool, sum::bool, level::nat) =
{ asid = curAsid64()
; match lookupTLB39(asid, vAddr, TLB39)
  { case Some(ent, idx) =>
    { if   checkPTEPermission(ac, priv, mxr, sum, pteBits(ent.pte.PTE_BITS))
      then { mark_log(LOG_ADDRTR, "TLB39 hit!")
           ; match updatePTEBits(pteBits(ent.pte.PTE_BITS), ac)
             { case None =>
                  Some(ent.pAddr || ZeroExtend(vAddr && ent.vAddrMask))
               case Some(pbits) =>
               { if   enable_dirty_update
                 then { var n_ent = ent
                      ; var tlb   = TLB39
                      -- update entry and TLB
                      ; n_ent.pte.PTE_BITS  <- &pbits
                      ; tlb([idx])          <- Some(n_ent)
                      ; TLB39               <- tlb
                      -- update memory
                      ; rawWriteData(ZeroExtend(n_ent.pteAddr), n_ent.&pte, 8)
                      -- done
                      ; Some(n_ent.pAddr || ZeroExtend(vAddr && n_ent.vAddrMask))
                      }
                 else { mark_log(LOG_ADDRTR, "tlb/pte needs dirty/accessed update!")
                      ; None
                      }
               }
             }
           }
      else { mark_log(LOG_ADDRTR, "TLB39 permission check failure")
           ; None
           }
    }
    case None =>
    { mark_log(LOG_ADDRTR, "TLB39 miss!")
    ; match walk39(vAddr, ac, priv, mxr, sum, curPTB39(), level, false)
      { case None =>
          None
        case Some(pAddr, pte, pteAddr, i, global) =>
        { match updatePTEBits(pteBits(pte.PTE_BITS), ac)
          { case None =>
            { TLB39 <- addToTLB39(asid, vAddr, pAddr, pte, pteAddr, i, global, TLB39)
            ; Some(pAddr)
            }
            case Some(pbits) =>
            { if   enable_dirty_update
              then { var pte_w = pte
                   ; pte_w.PTE_BITS <- &pbits
                   ; rawWriteData(ZeroExtend(pteAddr), &pte_w, 8)
                   ; TLB39 <- addToTLB39(asid, vAddr, pAddr, pte_w, pteAddr, i, global, TLB39)
                   ; Some(pAddr)
                   }
              else { mark_log(LOG_ADDRTR, "pte needs dirty/accessed update!")
                   ; None
                   }
            }
          }
        }
      }
    }
  }
}

-- address translation mode
--
-- The address translation mode is derived from satp.  How wide satp
-- is, and its mode, depend on the effective ISA derived from M_SXL.
-- However, we should not check M_SXL if we don't need to, since it
-- may not have a valid value if we are in pure M-mode.
SATP_Mode translationMode(priv::Privilege) =
    if   priv == Machine then Sbare -- no translation
    else { arch = architecture(MCSR.mstatus.M_SXL)
         ; match arch
           { case RV32  => if   satp32(SCSR.satp<31:0>).SATP32_MODE
                           then Sv32 else Sbare
             case RV64  => satpMode_ofbits(satp64(SCSR.satp).SATP64_MODE, arch)
             case RV128 => #UNDEFINED("Unsupported address translation arch: "
                                      : [arch]::string)
           }
         }

-- top-level address translation dispatcher
--
-- vAddr here is assumed to be appropriately formatted for the
-- current M_UXL.
pAddr option translateAddr(vAddr::regType, ac::accessType, ft::fetchType) =
{ priv = match ft
         { case Instruction => curPrivilege
           case Data        => if   MCSR.mstatus.M_MPRV
                               then privilege(MCSR.mstatus.M_MPP)
                               else curPrivilege
         }
; mxr  = MCSR.mstatus.M_MXR
; sum  = MCSR.mstatus.M_SUM
; mode = translationMode(priv)
; match mode
  { case Sbare  => Some(vAddr)  -- no translation

    case Sv32   => match translate32(vAddr<31:0>, ac, priv, mxr, sum, SV32_LEVELS - 1)
                   { case Some(pa32)  => Some(ZeroExtend(pa32))
                     case None        => None
                   }
    case Sv39   => match translate39(vAddr<38:0>, ac, priv, mxr, sum, SV39_LEVELS - 1)
                   { case Some(pa39)  => Some(ZeroExtend(pa39))
                     case None        => None
                   }

--  case Sv48   => translate64(vAddr, ft, ac, priv, 3)

    case     _  => None
  }
}

---------------------------------------------------------------------------
-- Load Reservation
---------------------------------------------------------------------------

bool matchLoadReservation(vAddr::vAddr) =
    IsSome(ReserveLoad) and ValOf(ReserveLoad) == vAddr

---------------------------------------------------------------------------
-- Control Flow
---------------------------------------------------------------------------

unit branchTo(newPC::regType) =
{ NextFetch             <- Some(BranchTo(newPC))
; Delta.mem_addr        <- Some(newPC)
; Delta.mem_access      <- Some(Execute)
}

unit noBranch(nextPC::regType) = ()

---------------------------------------------------------------------------
-- Extension checks
---------------------------------------------------------------------------

bool isFPEnabled()   = extStatus(MCSR.mstatus.M_FS) != Off

bool canDoFPSingle() = haveFPSingle() and isFPEnabled()
bool canDoFPDouble() = haveFPDouble() and isFPEnabled()
bool canDoAtomics()  = MCSR.misa.A

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
    if in32BitMode()
    then signalException(E_Illegal_Instr)
    else { temp = GPR(rs1) + SignExtend(imm)
         ; writeRD(rd, SignExtend(temp<31:0>))
         }

-----------------------------------
-- SLTI  rd, rs1, imm
-----------------------------------
define ArithI > SLTI(rd::reg, rs1::reg, imm::imm12) =
{ v1 = if in32BitMode() then SignExtend(GPR(rs1)<31:0>) else GPR(rs1)
; writeRD(rd, [v1 < SignExtend(imm)])
}

-----------------------------------
-- SLTIU rd, rs1, imm
-----------------------------------
define ArithI > SLTIU(rd::reg, rs1::reg, imm::imm12) =
{ v1 = if in32BitMode() then SignExtend(GPR(rs1)<31:0>) else GPR(rs1)
; writeRD(rd, [v1 <+ SignExtend(imm)])
}

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


-----------------------------------
-- SLLI  rd, rs1, imm
-----------------------------------
define Shift > SLLI(rd::reg, rs1::reg, imm::bits(6)) =
    if in32BitMode() and imm<5>
    then signalException(E_Illegal_Instr)
    else writeRD(rd, GPR(rs1) << [imm])

-----------------------------------
-- SRLI  rd, rs1, imm
-----------------------------------
define Shift > SRLI(rd::reg, rs1::reg, imm::bits(6)) =
    if in32BitMode() and imm<5>
    then signalException(E_Illegal_Instr)
    else { v1 = if in32BitMode() then ZeroExtend(GPR(rs1)<31:0>) else GPR(rs1)
         ; writeRD(rd, v1 >>+ [imm])
         }

-----------------------------------
-- SRAI  rd, rs1, imm
-----------------------------------
define Shift > SRAI(rd::reg, rs1::reg, imm::bits(6)) =
    if in32BitMode() and imm<5>
    then signalException(E_Illegal_Instr)
    else { v1 = if in32BitMode() then SignExtend(GPR(rs1)<31:0>) else GPR(rs1)
         ; writeRD(rd, v1 >> [imm])
         }

-----------------------------------
-- SLLIW rd, rs1, imm   (RV64I)
-----------------------------------
define Shift > SLLIW(rd::reg, rs1::reg, imm::bits(5)) =
    if in32BitMode()
    then signalException(E_Illegal_Instr)
    else writeRD(rd, SignExtend(GPR(rs1)<31:0> << [imm]))

-----------------------------------
-- SRLIW rd, rs1, imm   (RV64I)
-----------------------------------
define Shift > SRLIW(rd::reg, rs1::reg, imm::bits(5)) =
    if in32BitMode()
    then signalException(E_Illegal_Instr)
    else writeRD(rd, SignExtend(GPR(rs1)<31:0> >>+ [imm]))

-----------------------------------
-- SRAIW rd, rs1, imm   (RV64I)
-----------------------------------
define Shift > SRAIW(rd::reg, rs1::reg, imm::bits(5)) =
    if in32BitMode()
    then signalException(E_Illegal_Instr)
    else writeRD(rd, SignExtend(GPR(rs1)<31:0> >> [imm]))

-----------------------------------
-- LUI   rd, imm
-----------------------------------
define ArithI > LUI(rd::reg, imm::imm20) =
    writeRD(rd, SignExtend(imm : 0`12))

-----------------------------------
-- AUIPC rd, imm
-----------------------------------
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
    if in32BitMode()
    then signalException(E_Illegal_Instr)
    else writeRD(rd, SignExtend(GPR(rs1)<31:0> + GPR(rs2)<31:0>))

-----------------------------------
-- SUB   rd, rs1, rs2
-----------------------------------
define ArithR > SUB(rd::reg, rs1::reg, rs2::reg) =
    writeRD(rd, GPR(rs1) - GPR(rs2))

-----------------------------------
-- SUBW  rd, rs1, rs2   (RV64I)
-----------------------------------
define ArithR > SUBW(rd::reg, rs1::reg, rs2::reg) =
    if in32BitMode()
    then signalException(E_Illegal_Instr)
    else writeRD(rd, SignExtend(GPR(rs1)<31:0> - GPR(rs2)<31:0>))

-----------------------------------
-- SLT   rd, rs1, rs2
-----------------------------------
define ArithR > SLT(rd::reg, rs1::reg, rs2::reg) =
{ v1  = if in32BitMode() then SignExtend(GPR(rs1)<31:0>) else GPR(rs1)
; v2  = if in32BitMode() then SignExtend(GPR(rs2)<31:0>) else GPR(rs2)
; writeRD(rd, [v1 < v2])
}

-----------------------------------
-- SLTU  rd, rs1, rs2
-----------------------------------
define ArithR > SLTU(rd::reg, rs1::reg, rs2::reg) =
{ v1  = if in32BitMode() then ZeroExtend(GPR(rs1)<31:0>) else GPR(rs1)
; v2  = if in32BitMode() then ZeroExtend(GPR(rs2)<31:0>) else GPR(rs2)
; writeRD(rd, [v1 <+ v2])
}

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
    if in32BitMode()
    then writeRD(rd, GPR(rs1) << ZeroExtend(GPR(rs2)<4:0>))
    else writeRD(rd, GPR(rs1) << ZeroExtend(GPR(rs2)<5:0>))

-----------------------------------
-- SLLW  rd, rs1, rs2   (RV64I)
-----------------------------------
define Shift > SLLW(rd::reg, rs1::reg, rs2::reg) =
    if in32BitMode()
    then signalException(E_Illegal_Instr)
    else writeRD(rd, SignExtend(GPR(rs1)<31:0> << ZeroExtend(GPR(rs2)<4:0>)))

-----------------------------------
-- SRL   rd, rs1, rs2
-----------------------------------
define Shift > SRL(rd::reg, rs1::reg, rs2::reg) =
    if in32BitMode()
    then writeRD(rd, ZeroExtend(GPR(rs1)<31:0> >>+ ZeroExtend(GPR(rs2)<4:0>)))
    else writeRD(rd, GPR(rs1) >>+ ZeroExtend(GPR(rs2)<5:0>))

-----------------------------------
-- SRLW  rd, rs1, rs2   (RV64I)
-----------------------------------
define Shift > SRLW(rd::reg, rs1::reg, rs2::reg) =
    if in32BitMode()
    then signalException(E_Illegal_Instr)
    else writeRD(rd, SignExtend(GPR(rs1)<31:0> >>+ ZeroExtend(GPR(rs2)<4:0>)))

-----------------------------------
-- SRA   rd, rs1, rs2
-----------------------------------
define Shift > SRA(rd::reg, rs1::reg, rs2::reg) =
    if in32BitMode()
    then writeRD(rd, SignExtend(GPR(rs1)<31:0> >> ZeroExtend(GPR(rs2)<4:0>)))
    else writeRD(rd, GPR(rs1) >> ZeroExtend(GPR(rs2)<5:0>))

-----------------------------------
-- SRAW  rd, rs1, rs2   (RV64I)
-----------------------------------
define Shift > SRAW(rd::reg, rs1::reg, rs2::reg) =
    if in32BitMode()
    then signalException(E_Illegal_Instr)
    else writeRD(rd, SignExtend(GPR(rs1)<31:0> >> ZeroExtend(GPR(rs2)<4:0>)))

---------------------------------------------------------------------------
-- Multiply and Divide
---------------------------------------------------------------------------

-----------------------------------
-- MUL   rd, rs1, rs2
-----------------------------------
define MulDiv > MUL(rd::reg, rs1::reg, rs2::reg) =
    writeRD(rd, GPR(rs1) * GPR(rs2))

-----------------------------------
-- MULH  rd, rs1, rs2
-----------------------------------
define MulDiv > MULH(rd::reg, rs1::reg, rs2::reg) =
{ v1  = if in32BitMode() then SignExtend(GPR(rs1)<31:0>) else GPR(rs1)
; v2  = if in32BitMode() then SignExtend(GPR(rs2)<31:0>) else GPR(rs2)
; prod`128 = SignExtend(v1) * SignExtend(v2)
; res = if in32BitMode() then SignExtend(prod<63:32>) else SignExtend(prod<127:64>)
; writeRD(rd, res)
}

-----------------------------------
-- MULHU rd, rs1, rs2
-----------------------------------
define MulDiv > MULHU(rd::reg, rs1::reg, rs2::reg) =
{ v1  = if in32BitMode() then ZeroExtend(GPR(rs1)<31:0>) else ZeroExtend(GPR(rs1))
; v2  = if in32BitMode() then ZeroExtend(GPR(rs2)<31:0>) else ZeroExtend(GPR(rs2))
; prod`128 = v1 * v2
; res = if in32BitMode() then ZeroExtend(prod<63:32>) else prod<127:64>
; writeRD(rd, res)
}

-----------------------------------
-- MULHSU rd, rs1, rs2
-----------------------------------
define MulDiv > MULHSU(rd::reg, rs1::reg, rs2::reg) =
{ v1  = if in32BitMode() then SignExtend(GPR(rs1)<31:0>) else SignExtend(GPR(rs1))
; v2  = if in32BitMode() then ZeroExtend(GPR(rs2)<31:0>) else ZeroExtend(GPR(rs2))
; prod`128 = v1 * v2
; res = if in32BitMode() then SignExtend(prod<63:32>) else prod<127:64>
; writeRD(rd, res)
}

-----------------------------------
-- MULW  rd, rs1, rs2
-----------------------------------
define MulDiv > MULW(rd::reg, rs1::reg, rs2::reg) =
    if in32BitMode()
    then signalException(E_Illegal_Instr)
    else { prod`64 = SignExtend(GPR(rs1)<31:0> * GPR(rs2)<31:0>)
         ; writeRD(rd, SignExtend(prod<31:0>))
         }

-----------------------------------
-- DIV   rd, rs1, rs2
-----------------------------------
define MulDiv > DIV(rd::reg, rs1::reg, rs2::reg) =
    if GPR(rs2) == 0x0
    then writeRD(rd, SignExtend(1`1))
    else { minus_one::regType   = SignExtend(1`1)
         ; minus_max            = 0b1 << (Size(GPR(rs1)) - 1)
         ; if GPR(rs1) == minus_max and GPR(rs2) == minus_one
           then writeRD(rd, minus_max)
           else writeRD(rd, GPR(rs1) quot GPR(rs2))
         }

-----------------------------------
-- REM   rd, rs1, rs2
-----------------------------------
define MulDiv > REM(rd::reg, rs1::reg, rs2::reg) =
    if GPR(rs2) == 0x0
    then writeRD(rd, GPR(rs1))
    else { minus_one::regType   = SignExtend(1`1)
         ; minus_max            = 0b1 << (Size(GPR(rs1)) - 1)
         ; if GPR(rs1) == minus_max and GPR(rs2) == minus_one
           then writeRD(rd, 0)
           else writeRD(rd, GPR(rs1) rem GPR(rs2))
         }

-----------------------------------
-- DIVU  rd, rs1, rs2
-----------------------------------
define MulDiv > DIVU(rd::reg, rs1::reg, rs2::reg) =
{ v1 = if in32BitMode() then ZeroExtend(GPR(rs1)<31:0>) else GPR(rs1)
; v2 = if in32BitMode() then ZeroExtend(GPR(rs2)<31:0>) else GPR(rs2)
; if v2 == 0x0
  then writeRD(rd, SignExtend(1`1))
  else writeRD(rd, v1 div v2)
}

-----------------------------------
-- REMU  rd, rs1, rs2
-----------------------------------
define MulDiv > REMU(rd::reg, rs1::reg, rs2::reg) =
    if GPR(rs2) == 0x0
    then writeRD(rd, GPR(rs1))
    else writeRD(rd, GPR(rs1) mod GPR(rs2))

-----------------------------------
-- DIVW  rd, rs1, rs2
-----------------------------------
define MulDiv > DIVW(rd::reg, rs1::reg, rs2::reg) =
    if in32BitMode()
    then signalException(E_Illegal_Instr)
    else { s1 = GPR(rs1)<31:0>
         ; s2 = GPR(rs2)<31:0>
         ; if s2 == 0x0
           then writeRD(rd, SignExtend(1`1))
           else { minus_one::word    = SignExtend(1`1)
                ; minus_max          = 0b1 << (Size(s1) - 1)
                ; if s1 == minus_max and s2 == minus_one
                  then writeRD(rd, SignExtend(minus_max))
                  else writeRD(rd, SignExtend(s1 quot s2))
                }
         }

-----------------------------------
-- REMW  rd, rs1, rs2
-----------------------------------
define MulDiv > REMW(rd::reg, rs1::reg, rs2::reg) =
    if in32BitMode()
    then signalException(E_Illegal_Instr)
    else { s1 = GPR(rs1)<31:0>
         ; s2 = GPR(rs2)<31:0>
         ; if s2 == 0x0
           then writeRD(rd, SignExtend(s1))
           else writeRD(rd, SignExtend(s1 rem s2))
         }

-----------------------------------
-- DIVUW rd, rs1, rs2
-----------------------------------
define MulDiv > DIVUW(rd::reg, rs1::reg, rs2::reg) =
    if in32BitMode()
    then signalException(E_Illegal_Instr)
    else { s1 = GPR(rs1)<31:0>
         ; s2 = GPR(rs2)<31:0>
         ; if s2 == 0x0
           then writeRD(rd, SignExtend(1`1))
           else writeRD(rd, SignExtend(s1 div s2))
         }

-----------------------------------
-- REMUW rd, rs1, rs2
-----------------------------------
define MulDiv > REMUW(rd::reg, rs1::reg, rs2::reg) =
    if in32BitMode()
    then signalException(E_Illegal_Instr)
    else { s1 = GPR(rs1)<31:0>
         ; s2 = GPR(rs2)<31:0>
         ; if s2 == 0x0
           then writeRD(rd, SignExtend(s1))
           else writeRD(rd, SignExtend(s1 mod s2))
         }

---------------------------------------------------------------------------
-- Control Transfer Instructions
---------------------------------------------------------------------------

-- Unconditional jumps

-----------------------------------
-- JAL   rd, offs
-----------------------------------
define Branch > JAL(rd::reg, imm::imm20) =
{ addr = PC + SignExtend(imm) << 1
; if addr<1:0> != 0
  then signalAddressException(E_Fetch_Addr_Align, addr)
  else { writeRD(rd, PC + 4)
       ; branchTo(addr)
       }
}

-----------------------------------
-- JALR  rd, rs1, imm
-----------------------------------
define Branch > JALR(rd::reg, rs1::reg, imm::imm12) =
{ addr = (GPR(rs1) + SignExtend(imm)) && SignExtend('10')
; if addr<1:0> != 0
  then signalAddressException(E_Fetch_Addr_Align, addr)
  else { writeRD(rd, PC + 4)
       ; branchTo(addr)
       }
}

-- conditional branches

-----------------------------------
-- BEQ   rs1, rs2, offs
-----------------------------------
define Branch > BEQ(rs1::reg, rs2::reg, offs::imm12) =
{ v1 = if in32BitMode() then SignExtend(GPR(rs1)<31:0>) else GPR(rs1)
; v2 = if in32BitMode() then SignExtend(GPR(rs2)<31:0>) else GPR(rs2)
; if v1 == v2
  then branchTo(PC + (SignExtend(offs) << 1))
  else noBranch(PC + 4)
}

-----------------------------------
-- BNE   rs1, rs2, offs
-----------------------------------
define Branch > BNE(rs1::reg, rs2::reg, offs::imm12) =
{ v1 = if in32BitMode() then SignExtend(GPR(rs1)<31:0>) else GPR(rs1)
; v2 = if in32BitMode() then SignExtend(GPR(rs2)<31:0>) else GPR(rs2)
; if v1 <> v2
  then branchTo(PC + (SignExtend(offs) << 1))
  else noBranch(PC + 4)
}

-----------------------------------
-- BLT   rs1, rs2, offs
-----------------------------------
define Branch > BLT(rs1::reg, rs2::reg, offs::imm12) =
{ v1 = if in32BitMode() then SignExtend(GPR(rs1)<31:0>) else GPR(rs1)
; v2 = if in32BitMode() then SignExtend(GPR(rs2)<31:0>) else GPR(rs2)
; if v1 < v2
  then branchTo(PC + (SignExtend(offs) << 1))
  else noBranch(PC + 4)
}

-----------------------------------
-- BLTU  rs1, rs2, offs
-----------------------------------
define Branch > BLTU(rs1::reg, rs2::reg, offs::imm12) =
{ v1 = if in32BitMode() then SignExtend(GPR(rs1)<31:0>) else GPR(rs1)
; v2 = if in32BitMode() then SignExtend(GPR(rs2)<31:0>) else GPR(rs2)
; if v1 <+ v2
  then branchTo(PC + (SignExtend(offs) << 1))
  else noBranch(PC + 4)
}

-----------------------------------
-- BGE   rs1, rs2, offs
-----------------------------------
define Branch > BGE(rs1::reg, rs2::reg, offs::imm12) =
{ v1 = if in32BitMode() then SignExtend(GPR(rs1)<31:0>) else GPR(rs1)
; v2 = if in32BitMode() then SignExtend(GPR(rs2)<31:0>) else GPR(rs2)
; if v1 >= v2
  then branchTo(PC + (SignExtend(offs) << 1))
  else noBranch(PC + 4)
}

-----------------------------------
-- BGEU  rs1, rs2, offs
-----------------------------------
define Branch > BGEU(rs1::reg, rs2::reg, offs::imm12) =
{ v1 = if in32BitMode() then SignExtend(GPR(rs1)<31:0>) else GPR(rs1)
; v2 = if in32BitMode() then SignExtend(GPR(rs2)<31:0>) else GPR(rs2)
; if v1 >=+ v2
  then branchTo(PC + (SignExtend(offs) << 1))
  else noBranch(PC + 4)
}

---------------------------------------------------------------------------
-- Load and Store Instructions
---------------------------------------------------------------------------

-----------------------------------
-- LW    rd, rs1, offs
-----------------------------------
define Load > LW(rd::reg, rs1::reg, offs::imm12) =
{ vAddr = GPR(rs1) + SignExtend(offs)
; match translateAddr(vAddr, Read, Data)
  { case Some(pAddr) => { val = SignExtend(rawReadData(pAddr)<31:0>)
                        ; writeRD(rd, val)
                        ; recordLoad(vAddr, val, WORD)
                        }
    case None        => signalAddressException(E_Load_Page_Fault, vAddr)
  }
}

-----------------------------------
-- LWU   rd, rs1, offs  (RV64I)
-----------------------------------
define Load > LWU(rd::reg, rs1::reg, offs::imm12) =
{ if in32BitMode()
  then signalException(E_Illegal_Instr)
  else { vAddr = GPR(rs1) + SignExtend(offs)
       ; match translateAddr(vAddr, Read, Data)
         { case Some(pAddr) => { val = ZeroExtend(rawReadData(pAddr)<31:0>)
                               ; writeRD(rd, val)
                               ; recordLoad(vAddr, val, WORD)
                               }
           case None        => signalAddressException(E_Load_Page_Fault, vAddr)
         }
       }
}

-----------------------------------
-- LH    rd, rs1, offs
-----------------------------------
define Load > LH(rd::reg, rs1::reg, offs::imm12) =
{ vAddr = GPR(rs1) + SignExtend(offs)
; match translateAddr(vAddr, Read, Data)
  { case Some(pAddr) => { val = SignExtend(rawReadData(pAddr)<15:0>)
                        ; writeRD(rd, val)
                        ; recordLoad(vAddr, val, HALFWORD)
                        }
    case None        => signalAddressException(E_Load_Page_Fault, vAddr)
  }
}

-----------------------------------
-- LHU   rd, rs1, offs
-----------------------------------
define Load > LHU(rd::reg, rs1::reg, offs::imm12) =
{ vAddr = GPR(rs1) + SignExtend(offs)
; match translateAddr(vAddr, Read, Data)
  { case Some(pAddr) => { val = ZeroExtend(rawReadData(pAddr)<15:0>)
                        ; writeRD(rd, val)
                        ; recordLoad(vAddr, val, HALFWORD)
                        }
    case None        => signalAddressException(E_Load_Page_Fault, vAddr)
  }
}

-----------------------------------
-- LB    rd, rs1, offs
-----------------------------------
define Load > LB(rd::reg, rs1::reg, offs::imm12) =
{ vAddr = GPR(rs1) + SignExtend(offs)
; match translateAddr(vAddr, Read, Data)
  { case Some(pAddr) => { val = SignExtend(rawReadData(pAddr)<7:0>)
                        ; writeRD(rd, val)
                        ; recordLoad(vAddr, val, BYTE)
                        }
    case None        => signalAddressException(E_Load_Page_Fault, vAddr)
  }
}

-----------------------------------
-- LBU   rd, rs1, offs
-----------------------------------
define Load > LBU(rd::reg, rs1::reg, offs::imm12) =
{ vAddr = GPR(rs1) + SignExtend(offs)
; match translateAddr(vAddr, Read, Data)
  { case Some(pAddr) => { val = ZeroExtend(rawReadData(pAddr)<7:0>)
                        ; writeRD(rd, val)
                        ; recordLoad(vAddr, val, BYTE)
                        }
    case None        => signalAddressException(E_Load_Page_Fault, vAddr)
  }
}

-----------------------------------
-- LD    rd, rs1, offs  (RV64I)
-----------------------------------
define Load > LD(rd::reg, rs1::reg, offs::imm12) =
    if in32BitMode()
    then signalException(E_Illegal_Instr)
    else { vAddr = GPR(rs1) + SignExtend(offs)
         ; match translateAddr(vAddr, Read, Data)
           { case Some(pAddr) => { val = rawReadData(pAddr)
                                 ; writeRD(rd, val)
                                 ; recordLoad(vAddr, val, DOUBLEWORD)
                                 }
             case None        => signalAddressException(E_Load_Page_Fault, vAddr)
           }
         }

-----------------------------------
-- SW    rs1, rs2, offs
-----------------------------------
define Store > SW(rs1::reg, rs2::reg, offs::imm12) =
{ vAddr = GPR(rs1) + SignExtend(offs)
; match translateAddr(vAddr, Write, Data)
  { case Some(pAddr) => { data = GPR(rs2)
                        ; rawWriteData(pAddr, data, 4)
                        ; recordStore(vAddr, data, WORD)
                        }
    case None        => signalAddressException(E_SAMO_Page_Fault, vAddr)
  }
}

-----------------------------------
-- SH    rs1, rs2, offs
-----------------------------------
define Store > SH(rs1::reg, rs2::reg, offs::imm12) =
{ vAddr = GPR(rs1) + SignExtend(offs)
; match translateAddr(vAddr, Write, Data)
  { case Some(pAddr) => { data = GPR(rs2)
                        ; rawWriteData(pAddr, data, 2)
                        ; recordStore(vAddr, data, HALFWORD)
                        }
    case None        => signalAddressException(E_SAMO_Page_Fault, vAddr)
  }
}

-----------------------------------
-- SB    rs1, rs2, offs
-----------------------------------
define Store > SB(rs1::reg, rs2::reg, offs::imm12) =
{ vAddr = GPR(rs1) + SignExtend(offs)
; match translateAddr(vAddr, Write, Data)
  { case Some(pAddr) => { data = GPR(rs2)
                        ; rawWriteData(pAddr, data, 1)
                        ; recordStore(vAddr, data, BYTE)
                        }
    case None        => signalAddressException(E_SAMO_Page_Fault, vAddr)
  }
}

-----------------------------------
-- SD    rs1, rs2, offs (RV64I)
-----------------------------------
define Store > SD(rs1::reg, rs2::reg, offs::imm12) =
    if in32BitMode()
    then signalException(E_Illegal_Instr)
    else { vAddr = GPR(rs1) + SignExtend(offs)
         ; match translateAddr(vAddr, Write, Data)
           { case Some(pAddr) => { data = GPR(rs2)
                                 ; rawWriteData(pAddr, data, 8)
                                 ; recordStore(vAddr, data, DOUBLEWORD)
                                 }
             case None        => signalAddressException(E_SAMO_Page_Fault, vAddr)
           }
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
{ vAddr = GPR(rs1)
; if vAddr<1:0> != 0
  then signalAddressException(E_SAMO_Addr_Align, vAddr)
  else match translateAddr(vAddr, Read, Data)
       { case Some(pAddr) => { writeRD(rd, SignExtend(rawReadData(pAddr)<31:0>))
                             ; ReserveLoad  <- Some(vAddr)
                             }
         case None        => signalAddressException(E_Load_Page_Fault, vAddr)
       }
}

-----------------------------------
-- LR.D [aq,rl] rd, rs1
-----------------------------------
define AMO > LR_D(aq::amo, rl::amo, rd::reg, rs1::reg) =
    if in32BitMode()
    then signalException(E_Illegal_Instr)
    else { vAddr = GPR(rs1)
         ; if vAddr<2:0> != 0
           then signalAddressException(E_SAMO_Addr_Align, vAddr)
           else match translateAddr(vAddr, Read, Data)
                { case Some(pAddr) => { writeRD(rd, rawReadData(pAddr))
                                      ; ReserveLoad <- Some(vAddr)
                                      }
                  case None        => signalAddressException(E_Load_Page_Fault, vAddr)
                }
         }

-----------------------------------
-- SC.W [aq,rl] rd, rs1, rs2
-----------------------------------
define AMO > SC_W(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
{ vAddr = GPR(rs1)
; if vAddr<1:0> != 0
  then signalAddressException(E_SAMO_Addr_Align, vAddr)
  else if not matchLoadReservation(vAddr)
       then writeRD(rd, 1)
       else match translateAddr(vAddr, Write, Data)
            { case Some(pAddr) => { data = GPR(rs2)
                                  ; rawWriteData(pAddr, data, 4)
                                  ; recordStore(vAddr, data, WORD)
                                  ; writeRD(rd, 0)
                                  ; ReserveLoad  <- None
                                  }
              case None        => signalAddressException(E_SAMO_Page_Fault, vAddr)
            }
}

-----------------------------------
-- SC.D [aq,rl] rd, rs1, rs2
-----------------------------------
define AMO > SC_D(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
    if in32BitMode()
    then signalException(E_Illegal_Instr)
    else { vAddr = GPR(rs1)
         ; if vAddr<2:0> != 0
           then signalAddressException(E_SAMO_Addr_Align, vAddr)
           else if not matchLoadReservation(vAddr)
                then writeRD(rd, 1)
                else match translateAddr(vAddr, Write, Data)
                     { case Some(pAddr) => { data = GPR(rs2)
                                           ; rawWriteData(pAddr, data, 4)
                                           ; recordStore(vAddr, data, WORD)
                                           ; writeRD(rd, 0)
                                           ; ReserveLoad  <- None
                                           }
                       case None        => signalAddressException(E_SAMO_Page_Fault, vAddr)
                     }
         }

-----------------------------------
-- AMOSWAP.W [aq,rl] rd, rs1, rs2
-----------------------------------
define AMO > AMOSWAP_W(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
{ vAddr = GPR(rs1)
; if vAddr<1:0> != 0
  then signalAddressException(E_SAMO_Addr_Align, vAddr)
  else match translateAddr(vAddr, ReadWrite, Data)
       { case Some(pAddr) => { memv = SignExtend(rawReadData(pAddr)<31:0>)
                             ; data = GPR(rs2)
                             ; writeRD(rd, memv)
                             ; rawWriteData(pAddr, data, 4)
                             ; recordLoad(vAddr, memv, WORD)
                             ; recordAMOStore(vAddr, data, WORD)
                             }
         case None        => signalAddressException(E_SAMO_Page_Fault, vAddr)
       }
}

-----------------------------------
-- AMOSWAP.D [aq,rl] rd, rs1, rs2
-----------------------------------
define AMO > AMOSWAP_D(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
{ vAddr = GPR(rs1)
; if vAddr<2:0> != 0
  then signalAddressException(E_SAMO_Addr_Align, vAddr)
  else match translateAddr(vAddr, ReadWrite, Data)
       { case Some(pAddr) => { memv = rawReadData(pAddr)
                             ; data = GPR(rs2)
                             ; writeRD(rd, memv)
                             ; rawWriteData(pAddr, data, 8)
                             ; recordLoad(vAddr, memv, DOUBLEWORD)
                             ; recordAMOStore(vAddr, data, DOUBLEWORD)
                             }
         case None        => signalAddressException(E_SAMO_Page_Fault, vAddr)
       }
}

-----------------------------------
-- AMOADD.W [aq,rl] rd, rs1, rs2
-----------------------------------
define AMO > AMOADD_W(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
{ vAddr = GPR(rs1)
; if vAddr<1:0> != 0
  then signalAddressException(E_SAMO_Addr_Align, vAddr)
  else match translateAddr(vAddr, ReadWrite, Data)
       { case Some(pAddr) => { memv = SignExtend(rawReadData(pAddr)<31:0>)
                             ; data = GPR(rs2)
                             ; val  = data + memv
                             ; writeRD(rd, memv)
                             ; rawWriteData(pAddr, val, 4)
                             ; recordLoad(vAddr, memv, WORD)
                             ; recordAMOStore(vAddr, val, WORD)
                             }
         case None        => signalAddressException(E_SAMO_Page_Fault, vAddr)
       }
}

-----------------------------------
-- AMOADD.D [aq,rl] rd, rs1, rs2
-----------------------------------
define AMO > AMOADD_D(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
{ vAddr = GPR(rs1)
; if vAddr<2:0> != 0
  then signalAddressException(E_SAMO_Addr_Align, vAddr)
  else match translateAddr(vAddr, ReadWrite, Data)
       { case Some(pAddr) => { memv = rawReadData(pAddr)
                             ; data = GPR(rs2)
                             ; val  = data + memv
                             ; writeRD(rd, memv)
                             ; rawWriteData(pAddr, val, 8)
                             ; recordLoad(vAddr, memv, DOUBLEWORD)
                             ; recordAMOStore(vAddr, val, DOUBLEWORD)
                             }
         case None        => signalAddressException(E_SAMO_Page_Fault, vAddr)
       }
}

-----------------------------------
-- AMOXOR.W [aq,rl] rd, rs1, rs2
-----------------------------------
define AMO > AMOXOR_W(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
{ vAddr = GPR(rs1)
; if vAddr<1:0> != 0
  then signalAddressException(E_SAMO_Addr_Align, vAddr)
  else match translateAddr(vAddr, ReadWrite, Data)
       { case Some(pAddr) => { memv = SignExtend(rawReadData(pAddr)<31:0>)
                             ; data = GPR(rs2)
                             ; val  = data ?? memv
                             ; writeRD(rd, memv)
                             ; rawWriteData(pAddr, val, 4)
                             ; recordLoad(vAddr, memv, WORD)
                             ; recordAMOStore(vAddr, val, WORD)
                             }
         case None        => signalAddressException(E_SAMO_Page_Fault, vAddr)
       }
}

-----------------------------------
-- AMOXOR.D [aq,rl] rd, rs1, rs2
-----------------------------------
define AMO > AMOXOR_D(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
{ vAddr = GPR(rs1)
; if vAddr<2:0> != 0
  then signalAddressException(E_SAMO_Addr_Align, vAddr)
  else match translateAddr(vAddr, ReadWrite, Data)
       { case Some(pAddr) => { memv = rawReadData(pAddr)
                             ; data = GPR(rs2)
                             ; val  = data ?? memv
                             ; writeRD(rd, memv)
                             ; rawWriteData(pAddr, val, 8)
                             ; recordLoad(vAddr, memv, DOUBLEWORD)
                             ; recordAMOStore(vAddr, val, DOUBLEWORD)
                             }
         case None        => signalAddressException(E_SAMO_Page_Fault, vAddr)
       }
}

-----------------------------------
-- AMOAND.W [aq,rl] rd, rs1, rs2
-----------------------------------
define AMO > AMOAND_W(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
{ vAddr = GPR(rs1)
; if vAddr<1:0> != 0
  then signalAddressException(E_SAMO_Addr_Align, vAddr)
  else match translateAddr(vAddr, ReadWrite, Data)
       { case Some(pAddr) => { memv = SignExtend(rawReadData(pAddr)<31:0>)
                             ; data = GPR(rs2)
                             ; val  = data && memv
                             ; writeRD(rd, memv)
                             ; rawWriteData(pAddr, val, 4)
                             ; recordLoad(vAddr, memv, WORD)
                             ; recordAMOStore(vAddr, val, WORD)
                             }
         case None        => signalAddressException(E_SAMO_Page_Fault, vAddr)
       }
}

-----------------------------------
-- AMOAND.D [aq,rl] rd, rs1, rs2
-----------------------------------
define AMO > AMOAND_D(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
{ vAddr = GPR(rs1)
; if vAddr<2:0> != 0
  then signalAddressException(E_SAMO_Addr_Align, vAddr)
  else match translateAddr(vAddr, ReadWrite, Data)
       { case Some(pAddr) => { memv = rawReadData(pAddr)
                             ; data = GPR(rs2)
                             ; val  = data && memv
                             ; writeRD(rd, memv)
                             ; rawWriteData(pAddr, val, 8)
                             ; recordLoad(vAddr, memv, DOUBLEWORD)
                             ; recordAMOStore(vAddr, val, DOUBLEWORD)
                             }
         case None        => signalAddressException(E_SAMO_Page_Fault, vAddr)
       }
}

-----------------------------------
-- AMOOR.W [aq,rl] rd, rs1, rs2
-----------------------------------
define AMO > AMOOR_W(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
{ vAddr = GPR(rs1)
; if vAddr<1:0> != 0
  then signalAddressException(E_SAMO_Addr_Align, vAddr)
  else match translateAddr(vAddr, ReadWrite, Data)
       { case Some(pAddr) => { memv = SignExtend(rawReadData(pAddr)<31:0>)
                             ; data = GPR(rs2)
                             ; val  = data || memv
                             ; writeRD(rd, memv)
                             ; rawWriteData(pAddr, val, 4)
                             ; recordLoad(vAddr, memv, WORD)
                             ; recordAMOStore(vAddr, val, WORD)
                             }
         case None        => signalAddressException(E_SAMO_Page_Fault, vAddr)
       }
}

-----------------------------------
-- AMOOR.D [aq,rl] rd, rs1, rs2
-----------------------------------
define AMO > AMOOR_D(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
{ vAddr = GPR(rs1)
; if vAddr<2:0> != 0
  then signalAddressException(E_SAMO_Addr_Align, vAddr)
  else match translateAddr(vAddr, ReadWrite, Data)
       { case Some(pAddr) => { memv = rawReadData(pAddr)
                             ; data = GPR(rs2)
                             ; val  = data || memv
                             ; writeRD(rd, memv)
                             ; rawWriteData(pAddr, val, 8)
                             ; recordLoad(vAddr, memv, DOUBLEWORD)
                             ; recordAMOStore(vAddr, val, DOUBLEWORD)
                             }
         case None        => signalAddressException(E_SAMO_Page_Fault, vAddr)
       }
}

-----------------------------------
-- AMOMIN.W [aq,rl] rd, rs1, rs2
-----------------------------------
define AMO > AMOMIN_W(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
{ vAddr = GPR(rs1)
; if vAddr<1:0> != 0
  then signalAddressException(E_SAMO_Addr_Align, vAddr)
  else match translateAddr(vAddr, ReadWrite, Data)
       { case Some(pAddr) => { memv = SignExtend(rawReadData(pAddr)<31:0>)
                             ; data = GPR(rs2)
                             ; val  = SignedMin(data, memv)
                             ; writeRD(rd, memv)
                             ; rawWriteData(pAddr, val, 4)
                             ; recordLoad(vAddr, memv, WORD)
                             ; recordAMOStore(vAddr, val, WORD)
                             }
         case None        => signalAddressException(E_SAMO_Page_Fault, vAddr)
       }
}

-----------------------------------
-- AMOMIN.D [aq,rl] rd, rs1, rs2
-----------------------------------
define AMO > AMOMIN_D(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
{ vAddr = GPR(rs1)
; if vAddr<2:0> != 0
  then signalAddressException(E_SAMO_Addr_Align, vAddr)
  else match translateAddr(vAddr, ReadWrite, Data)
       { case Some(pAddr) => { memv = rawReadData(pAddr)
                             ; data = GPR(rs2)
                             ; val  = SignedMin(data, memv)
                             ; writeRD(rd, memv)
                             ; rawWriteData(pAddr, val, 8)
                             ; recordLoad(vAddr, memv, DOUBLEWORD)
                             ; recordAMOStore(vAddr, val, DOUBLEWORD)
                             }
         case None        => signalAddressException(E_SAMO_Page_Fault, vAddr)
       }
}

-----------------------------------
-- AMOMAX.W [aq,rl] rd, rs1, rs2
-----------------------------------
define AMO > AMOMAX_W(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
{ vAddr = GPR(rs1)
; if vAddr<1:0> != 0
  then signalAddressException(E_SAMO_Addr_Align, vAddr)
  else match translateAddr(vAddr, ReadWrite, Data)
       { case Some(pAddr) => { memv = SignExtend(rawReadData(pAddr)<31:0>)
                             ; data = GPR(rs2)
                             ; val  = SignedMax(data, memv)
                             ; writeRD(rd, memv)
                             ; rawWriteData(pAddr, val, 4)
                             ; recordLoad(vAddr, memv, WORD)
                             ; recordAMOStore(vAddr, val, WORD)
                             }
         case None        => signalAddressException(E_SAMO_Page_Fault, vAddr)
       }
}

-----------------------------------
-- AMOMAX.D [aq,rl] rd, rs1, rs2
-----------------------------------
define AMO > AMOMAX_D(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
{ vAddr = GPR(rs1)
; if vAddr<2:0> != 0
  then signalAddressException(E_SAMO_Addr_Align, vAddr)
  else match translateAddr(vAddr, ReadWrite, Data)
       { case Some(pAddr) => { memv = rawReadData(pAddr)
                             ; data = GPR(rs2)
                             ; val  = SignedMax(data, memv)
                             ; writeRD(rd, memv)
                             ; rawWriteData(pAddr, val, 8)
                             ; recordLoad(vAddr, memv, DOUBLEWORD)
                             ; recordAMOStore(vAddr, val, DOUBLEWORD)
                             }
         case None        => signalAddressException(E_SAMO_Page_Fault, vAddr)
       }
}

-----------------------------------
-- AMOMINU.W [aq,rl] rd, rs1, rs2
-----------------------------------
define AMO > AMOMINU_W(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
{ vAddr = GPR(rs1)
; if vAddr<1:0> != 0
  then signalAddressException(E_SAMO_Addr_Align, vAddr)
  else match translateAddr(vAddr, ReadWrite, Data)
       { case Some(pAddr) => { memv = SignExtend(rawReadData(pAddr)<31:0>)
                             ; data = GPR(rs2)
                             ; val  = Min(data, memv)
                             ; writeRD(rd, memv)
                             ; rawWriteData(pAddr, val, 4)
                             ; recordLoad(vAddr, memv, WORD)
                             ; recordAMOStore(vAddr, val, WORD)
                             }
         case None        => signalAddressException(E_SAMO_Page_Fault, vAddr)
       }
}

-----------------------------------
-- AMOMINU.D [aq,rl] rd, rs1, rs2
-----------------------------------
define AMO > AMOMINU_D(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
{ vAddr = GPR(rs1)
; if vAddr<2:0> != 0
  then signalAddressException(E_SAMO_Addr_Align, vAddr)
  else match translateAddr(vAddr, ReadWrite, Data)
       { case Some(pAddr) => { memv = rawReadData(pAddr)
                             ; data = GPR(rs2)
                             ; val  = Min(data, memv)
                             ; writeRD(rd, memv)
                             ; rawWriteData(pAddr, val, 8)
                             ; recordLoad(vAddr, memv, DOUBLEWORD)
                             ; recordAMOStore(vAddr, val, DOUBLEWORD)
                             }
         case None        => signalAddressException(E_SAMO_Page_Fault, vAddr)
       }
}

-----------------------------------
-- AMOMAXU.W [aq,rl] rd, rs1, rs2
-----------------------------------
define AMO > AMOMAXU_W(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
{ vAddr = GPR(rs1)
; if vAddr<1:0> != 0
  then signalAddressException(E_SAMO_Addr_Align, vAddr)
  else match translateAddr(vAddr, ReadWrite, Data)
       { case Some(pAddr) => { memv = SignExtend(rawReadData(pAddr)<31:0>)
                             ; data = GPR(rs2)
                             ; val  = Max(data, memv)
                             ; writeRD(rd, memv)
                             ; rawWriteData(pAddr, val, 4)
                             ; recordLoad(vAddr, memv, WORD)
                             ; recordAMOStore(vAddr, val, WORD)
                             }
         case None        => signalAddressException(E_SAMO_Page_Fault, vAddr)
       }
}

-----------------------------------
-- AMOMAXU.D [aq,rl] rd, rs1, rs2
-----------------------------------
define AMO > AMOMAXU_D(aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
{ vAddr = GPR(rs1)
; if vAddr<2:0> != 0
  then signalAddressException(E_SAMO_Addr_Align, vAddr)
  else match translateAddr(vAddr, ReadWrite, Data)
       { case Some(pAddr) => { memv = rawReadData(pAddr)
                             ; data = GPR(rs2)
                             ; val  = Max(data, memv)
                             ; writeRD(rd, memv)
                             ; rawWriteData(pAddr, val, 8)
                             ; recordLoad(vAddr, memv, DOUBLEWORD)
                             ; recordAMOStore(vAddr, val, DOUBLEWORD)
                             }
         case None        => signalAddressException(E_SAMO_Page_Fault, vAddr)
       }
}


---------------------------------------------------------------------------
-- Floating Point Instructions (Single Precision)
---------------------------------------------------------------------------

-- Load/Store

-----------------------------------
-- FLW   rd, rs2, offs
-----------------------------------

define FPLoad > FLW(rd::reg, rs1::reg, offs::imm12) =
{ if   not canDoFPSingle()
  then signalException(E_Illegal_Instr)
  else { vAddr = GPR(rs1) + SignExtend(offs)
       ; match translateAddr(vAddr, Read, Data)
         { case Some(pAddr) => { val = rawReadData(pAddr)<31:0>
                               ; writeFPRS(rd, val)
                               ; recordLoad(vAddr, ZeroExtend(val), WORD)
                               }
           case None        => signalAddressException(E_Load_Page_Fault, vAddr)
         }
       }
}

-----------------------------------
-- FSW   rs1, rs2, offs
-----------------------------------

define FPStore > FSW(rs1::reg, rs2::reg, offs::imm12) =
{ if   not canDoFPSingle()
  then signalException(E_Illegal_Instr)
  else { vAddr = GPR(rs1) + SignExtend(offs)
       ; match translateAddr(vAddr, Write, Data)
                   { case Some(pAddr) => { data = FPRS(rs2)
                                         ; rawWriteData(pAddr, ZeroExtend(data), 4)
                                         ; recordStore(vAddr, ZeroExtend(data), WORD)
                                         }
                     case None        => signalAddressException(E_SAMO_Page_Fault, vAddr)
                   }
       }
}

-- Computational

-- TODO: Check for underflow after all rounding

-----------------------------------
-- FADD.S   rd, rs1, rs2
-----------------------------------

define FArith > FADD_S(rd::reg, rs1::reg, rs2::reg, fprnd::fprnd) =
{ match round(fprnd), canDoFPSingle()
  { case Some(r), true  => writeFPRS(rd, FP32_Add(r, FPRS(rs1), FPRS(rs2)))
    case       _,_      => signalException(E_Illegal_Instr)
  }
}

-----------------------------------
-- FSUB.S   rd, rs1, rs2
-----------------------------------

define FArith > FSUB_S(rd::reg, rs1::reg, rs2::reg, fprnd::fprnd) =
{ match round(fprnd), canDoFPSingle()
  { case Some(r), true  => writeFPRS(rd, FP32_Sub(r, FPRS(rs1), FPRS(rs2)))
    case       _,_      => signalException(E_Illegal_Instr)
  }
}

-----------------------------------
-- FMUL.S   rd, rs1, rs2
-----------------------------------

define FArith > FMUL_S(rd::reg, rs1::reg, rs2::reg, fprnd::fprnd) =
{ match round(fprnd), canDoFPSingle()
  { case Some(r), true  => writeFPRS(rd, FP32_Mul(r, FPRS(rs1), FPRS(rs2)))
    case       _,_      => signalException(E_Illegal_Instr)
  }
}

-----------------------------------
-- FDIV.S   rd, rs1, rs2
-----------------------------------

define FArith > FDIV_S(rd::reg, rs1::reg, rs2::reg, fprnd::fprnd) =
{ match round(fprnd), canDoFPSingle()
  { case Some(r), true  => writeFPRS(rd, FP32_Div(r, FPRS(rs1), FPRS(rs2)))
    case       _,_      => signalException(E_Illegal_Instr)
  }
}

-----------------------------------
-- FSQRT.S   rd, rs
-----------------------------------

define FArith > FSQRT_S(rd::reg, rs::reg, fprnd::fprnd) =
{ match round(fprnd), canDoFPSingle()
  { case Some(r), true  => writeFPRS(rd, FP32_Sqrt(r, FPRS(rs)))
    case       _,_      => signalException(E_Illegal_Instr)
  }
}

-----------------------------------
-- FMIN.S    rd, rs1, rs2
-----------------------------------
define FArith > FMIN_S(rd::reg, rs1::reg, rs2::reg) =
{ if   not canDoFPSingle()
  then signalException(E_Illegal_Instr)
  else { f1  = FPRS(rs1)
       ; f2  = FPRS(rs2)
       ; res = match FP32_Compare(f1, f2)
               { case FP_LT   => f1
                 case FP_EQ   => f1
                 case FP_GT   => f2
                 case FP_UN   => if (   (FP32_IsSignalingNan(f1) or FP32_IsSignalingNan(f2))
                                     or (f1 == RV32_CanonicalNan and f2 == RV32_CanonicalNan))
                                 then RV32_CanonicalNan
                                 else -- either f1 or f2 should be a quiet NaN
                                     if f1 == RV32_CanonicalNan then f2 else f1
               }
       ; writeFPRS(rd, res)
       }
}

-----------------------------------
-- FMAX.S    rd, rs1, rs2
-----------------------------------
define FArith > FMAX_S(rd::reg, rs1::reg, rs2::reg) =
{ if   not canDoFPSingle()
  then signalException(E_Illegal_Instr)
  else { f1  = FPRS(rs1)
       ; f2  = FPRS(rs2)
       ; res = match FP32_Compare(f1, f2)
               { case FP_LT   => f2
                 case FP_EQ   => f2
                 case FP_GT   => f1
                 case FP_UN   => if (   (FP32_IsSignalingNan(f1) or FP32_IsSignalingNan(f2))
                                     or (f1 == RV32_CanonicalNan and f2 == RV32_CanonicalNan))
                                 then RV32_CanonicalNan
                                 else -- either f1 or f2 should be a quiet NaN
                                     if f1 == RV32_CanonicalNan then f2 else f1
               }
       ; writeFPRS(rd, res)
       }
}

-----------------------------------
-- FMADD.S   rd, rs1, rs2, rs3
-----------------------------------

define FArith > FMADD_S(rd::reg, rs1::reg, rs2::reg, rs3::reg, fprnd::fprnd) =
{ match round(fprnd), canDoFPSingle()
  { case Some(r), true => writeFPRS(rd, FP32_Add(r, FP32_Mul(r, FPRS(rs1), FPRS(rs2)), FPRS(rs3)))
    case       _,_     => signalException(E_Illegal_Instr)
  }
}

-----------------------------------
-- FMSUB.S   rd, rs1, rs2, rs3
-----------------------------------

define FArith > FMSUB_S(rd::reg, rs1::reg, rs2::reg, rs3::reg, fprnd::fprnd) =
{ match round(fprnd), canDoFPSingle()
  { case Some(r), true => writeFPRS(rd, FP32_Sub(r, FP32_Mul(r, FPRS(rs1), FPRS(rs2)), FPRS(rs3)))
    case       _,_     => signalException(E_Illegal_Instr)
  }
}

-----------------------------------
-- FNMADD.S   rd, rs1, rs2, rs3
-----------------------------------

define FArith > FNMADD_S(rd::reg, rs1::reg, rs2::reg, rs3::reg, fprnd::fprnd) =
{ match round(fprnd), canDoFPSingle()
  { case Some(r), true  => writeFPRS(rd, FP32_Neg(FP32_Add(r, FP32_Mul(r, FPRS(rs1), FPRS(rs2)), FPRS(rs3))))
    case       _,_      => signalException(E_Illegal_Instr)
  }
}

-----------------------------------
-- FNMSUB.S   rd, rs1, rs2, rs3
-----------------------------------

define FArith > FNMSUB_S(rd::reg, rs1::reg, rs2::reg, rs3::reg, fprnd::fprnd) =
{ match round(fprnd), canDoFPSingle()
  { case Some(r), true  => writeFPRS(rd, FP32_Neg(FP32_Sub(r, FP32_Mul(r, FPRS(rs1), FPRS(rs2)), FPRS(rs3))))
    case       _,_      => signalException(E_Illegal_Instr)
  }
}

-- Conversions

-----------------------------------
-- FCVT.S.W   rd, rs
-----------------------------------

define FConv > FCVT_S_W(rd::reg, rs::reg, fprnd::fprnd) =
{ match round(fprnd), canDoFPSingle()
  { case Some(r), true  => writeFPRS(rd, FP32_FromInt(r, [GPR(rs)<31:0>]::int))
    case       _,_      => signalException(E_Illegal_Instr)
  }
}

-----------------------------------
-- FCVT.S.WU  rd, rs
-----------------------------------

define FConv > FCVT_S_WU(rd::reg, rs::reg, fprnd::fprnd) =
{ match round(fprnd), canDoFPSingle()
  { case Some(r), true  => writeFPRS(rd, FP32_FromInt(r, [0`1 : GPR(rs)<31:0>]::int))
    case       _,_      => signalException(E_Illegal_Instr)
  }
}

-----------------------------------
-- FCVT.W.S   rd, rs
-----------------------------------

define FConv > FCVT_W_S(rd::reg, rs::reg, fprnd::fprnd) =
{ match round(fprnd), canDoFPSingle()
  { case Some(r), true  => { inp = FPRS(rs)
                           ; val = ValOf(FP32_ToInt(r, inp))
                           ; res = if   FP32_IsNan(inp) or inp == FP32_PosInf
                                   then [0n2 ** 31 - 1]
                                   else if inp == FP32_NegInf
                                   then -[0n2 ** 31]
                                   else if val > 2 ** 31 - 1
                                   then [0n2 ** 31 - 1]
                                   else if val < -2 ** 31
                                   then -[0n2 ** 31]
                                   else [val]
                           ; writeRD(rd, res)
                    }
    case       _,_      => signalException(E_Illegal_Instr)
  }
}

-----------------------------------
-- FCVT.WU.S  rd, rs
-----------------------------------

define FConv > FCVT_WU_S(rd::reg, rs::reg, fprnd::fprnd) =
{ match round(fprnd), canDoFPSingle()
  { case Some(r), true  => { inp = FPRS(rs)
                           ; val = ValOf(FP32_ToInt(r, inp))
                           ; res = if   FP32_IsNan(inp) or inp == FP32_PosInf
                                   then [0n2 ** 32 - 1]
                                   else if inp == FP32_NegInf
                                   then 0x0
                                   else if val > 2 ** 32 - 1
                                   then [0n2 ** 32 - 1]
                                   else if val < 0
                                   then 0x0
                                   else [val]
                           ; writeRD(rd, res)
                           }
    case       _,_      => signalException(E_Illegal_Instr)
  }
}

-----------------------------------
-- FCVT.S.L   rd, rs
-----------------------------------

define FConv > FCVT_S_L(rd::reg, rs::reg, fprnd::fprnd) =
{ match round(fprnd), canDoFPSingle()
  { case Some(r), true  => writeFPRS(rd, FP32_FromInt(r, [GPR(rs)]::int))
    case       _,_      => signalException(E_Illegal_Instr)
  }
}

-----------------------------------
-- FCVT.S.LU  rd, rs
-----------------------------------

define FConv > FCVT_S_LU(rd::reg, rs::reg, fprnd::fprnd) =
{ match round(fprnd), canDoFPSingle()
  { case Some(r), true  => writeFPRS(rd, FP32_FromInt(r, [0`1 : GPR(rs)]::int))
    case       _,_      => signalException(E_Illegal_Instr)
  }
}

-----------------------------------
-- FCVT.L.S   rd, rs
-----------------------------------

define FConv > FCVT_L_S(rd::reg, rs::reg, fprnd::fprnd) =
{ match round(fprnd), canDoFPSingle()
  { case Some(r), true => { inp = FPRS(rs)
                          ; val = ValOf(FP32_ToInt(r, inp))
                          ; res = if   FP32_IsNan(inp) or inp == FP32_PosInf
                                  then [0n2 ** 63 - 1]
                                  else if inp == FP32_NegInf
                                  then -[0n2 ** 63]
                                  else if val > 2 ** 63 - 1
                                  then [0n2 ** 63 - 1]
                                  else if val < -2 ** 63
                                  then -[0n2 ** 63]
                                  else [val]
                          ; writeRD(rd, res)
                          }
    case      _,_       => signalException(E_Illegal_Instr)
  }
}

-----------------------------------
-- FCVT.LU.S  rd, rs
-----------------------------------

define FConv > FCVT_LU_S(rd::reg, rs::reg, fprnd::fprnd) =
{ match round(fprnd), canDoFPSingle()
  { case Some(r), true  => { inp = FPRS(rs)
                           ; val = ValOf(FP32_ToInt(r, inp))
                           ; res = if   FP32_IsNan(inp) or inp == FP32_PosInf
                                   then [0n2 ** 64 - 1]
                                   else if inp == FP32_NegInf
                                   then 0x0
                                   else if val > 2 ** 64 - 1
                                   then [0n2 ** 64 - 1]
                                   else if val < 0
                                   then 0x0
                                   else [val]
                           ; writeRD(rd, res)
                           }
    case      _,_       => signalException(E_Illegal_Instr)
  }
}

-- Sign injection

-----------------------------------
-- FSGNJ.S   rd, rs
-----------------------------------

define FConv > FSGNJ_S(rd::reg, rs1::reg, rs2::reg) =
{ if   not canDoFPSingle()
  then signalException(E_Illegal_Instr)
  else { f1 = FPRS(rs1)
       ; f2 = FPRS(rs2)
       ; writeFPRS(rd, ([FP32_Sign(f2)]::bits(1)):f1<30:0>)
       }
}

-----------------------------------
-- FSGNJN.S  rd, rs
-----------------------------------

define FConv > FSGNJN_S(rd::reg, rs1::reg, rs2::reg) =
{ if   not canDoFPSingle()
  then signalException(E_Illegal_Instr)
  else { f1 = FPRS(rs1)
       ; f2 = FPRS(rs2)
       ; writeFPRS(rd, ([!FP32_Sign(f2)]::bits(1)):f1<30:0>)
       }
}

-----------------------------------
-- FSGNJX.S  rd, rs
-----------------------------------

define FConv > FSGNJX_S(rd::reg, rs1::reg, rs2::reg) =
{ if   not canDoFPSingle()
  then signalException(E_Illegal_Instr)
  else { f1 = FPRS(rs1)
       ; f2 = FPRS(rs2)
       ; writeFPRS(rd, ([FP32_Sign(f2)]::bits(1) ?? [FP32_Sign(f1)]::bits(1)) : f1<30:0>)
       }
}

-- Movement

-----------------------------------
-- FMV.X.S   rd, rs
-----------------------------------

define FConv > FMV_X_S(rd::reg, rs::reg) =
    if   not canDoFPSingle()
    then signalException(E_Illegal_Instr)
    else writeRD(rd, SignExtend(FPRS(rs)))

-----------------------------------
-- FMV.S.X   rd, rs
-----------------------------------

define FConv > FMV_S_X(rd::reg, rs::reg) =
    if   not canDoFPSingle()
    then signalException(E_Illegal_Instr)
    else writeFPRS(rd, GPR(rs)<31:0>)

-- Comparisons

-----------------------------------
-- FEQ.S   rd, rs
-----------------------------------

define FArith > FEQ_S(rd::reg, rs1::reg, rs2::reg) =
{ if   not canDoFPSingle()
  then signalException(E_Illegal_Instr)
  else { f1  = FPRS(rs1)
       ; f2  = FPRS(rs2)
       ; if FP32_IsSignalingNan(f1) or FP32_IsSignalingNan(f2)
         then { writeRD(rd, 0x0)
              ; setFP_Invalid()
              }
         else { res = match FP32_Compare(f1, f2)
                      { case FP_LT => 0x0
                        case FP_EQ => 0x1
                        case FP_GT => 0x0
                        case FP_UN => 0x0
                      }
              ; writeRD(rd, res)
              }
       }
}

-----------------------------------
-- FLT.S   rd, rs
-----------------------------------

define FArith > FLT_S(rd::reg, rs1::reg, rs2::reg) =
{ if   not canDoFPSingle()
  then signalException(E_Illegal_Instr)
  else { f1  = FPRS(rs1)
       ; f2  = FPRS(rs2)
       ; if   FP32_IsNan(f1) or FP32_IsNan(f2)
         then { writeRD(rd, 0x0)
              ; setFP_Invalid()
              }
         else { res = match FP32_Compare(f1, f2)
                      { case FP_LT => 0x1
                        case FP_EQ => 0x0
                        case FP_GT => 0x0
                        case FP_UN => 0x0
                      }
              ; writeRD(rd, res)
              }
       }
}

-----------------------------------
-- FLE.S   rd, rs
-----------------------------------

define FArith > FLE_S(rd::reg, rs1::reg, rs2::reg) =
{ if   not canDoFPSingle()
  then signalException(E_Illegal_Instr)
  else { f1  = FPRS(rs1)
       ; f2  = FPRS(rs2)
       ; if   FP32_IsNan(f1) or FP32_IsNan(f2)
         then { writeRD(rd, 0x0)
              ; setFP_Invalid()
              }
         else { res = match FP32_Compare(f1, f2)
                      { case FP_LT => 0x1
                        case FP_EQ => 0x1
                        case FP_GT => 0x0
                        case FP_UN => 0x0
                      }
              ; writeRD(rd, res)
              }
       }
}

-- Classification

-----------------------------------
-- FCLASS.S  rd, rs
-----------------------------------

define FConv > FCLASS_S(rd::reg, rs::reg) =
{ if   not canDoFPSingle()
  then signalException(E_Illegal_Instr)
  else { var ret = 0x0`10
       ; val = FPRS(rs)
       ; ret<0> <- val == FP32_NegInf
       ; ret<1> <- FP32_Sign(val) and FP32_IsNormal(val)
       ; ret<2> <- FP32_Sign(val) and FP32_IsSubnormal(val)
       ; ret<3> <- val == FP32_NegZero
       ; ret<4> <- val == FP32_PosZero
       ; ret<5> <- !FP32_Sign(val) and FP32_IsSubnormal(val)
       ; ret<6> <- !FP32_Sign(val) and FP32_IsNormal(val)
       ; ret<7> <- val == FP32_PosInf
       ; ret<8> <- FP32_IsSignalingNan(val)
       ; ret<9> <- val == RV32_CanonicalNan
       ; writeRD(rd, ZeroExtend(ret))
       }
}

---------------------------------------------------------------------------
-- Floating Point Instructions (Double Precision)
---------------------------------------------------------------------------

-- Load/Store

-----------------------------------
-- FLD   rd, rs2, offs
-----------------------------------

define FPLoad > FLD(rd::reg, rs1::reg, offs::imm12) =
{ if   not canDoFPDouble()
  then signalException(E_Illegal_Instr)
  else { vAddr = GPR(rs1) + SignExtend(offs)
       ; match translateAddr(vAddr, Read, Data)
                   { case Some(pAddr) => { val = rawReadData(pAddr)
                                         ; writeFPRD(rd, val)
                                         ; recordLoad(vAddr, val, DOUBLEWORD)
                                         }
                     case None        => signalAddressException(E_Load_Page_Fault, vAddr)
                   }
       }
}

-----------------------------------
-- FSD   rs1, rs2, offs
-----------------------------------

define FPStore > FSD(rs1::reg, rs2::reg, offs::imm12) =
{ if   not canDoFPDouble()
  then signalException(E_Illegal_Instr)
  else { vAddr = GPR(rs1) + SignExtend(offs)
       ; match translateAddr(vAddr, Write, Data)
                   { case Some(pAddr) => { data = FPRD(rs2)
                                         ; rawWriteData(pAddr, data, 8)
                                         ; recordStore(vAddr, data, DOUBLEWORD)
                                         }
                     case None        => signalAddressException(E_SAMO_Page_Fault, vAddr)
                   }
       }
}

-- Computational

-- TODO: Check for underflow after all rounding

-----------------------------------
-- FADD.D   rd, rs1, rs2
-----------------------------------

define FArith > FADD_D(rd::reg, rs1::reg, rs2::reg, fprnd::fprnd) =
{ match round(fprnd), canDoFPDouble()
  { case Some(r), true  => writeFPRD(rd, FP64_Add(r, FPRD(rs1), FPRD(rs2)))
    case       _,_      => signalException(E_Illegal_Instr)
  }
}

-----------------------------------
-- FSUB.D   rd, rs1, rs2
-----------------------------------

define FArith > FSUB_D(rd::reg, rs1::reg, rs2::reg, fprnd::fprnd) =
{ match round(fprnd), canDoFPDouble()
  { case Some(r), true  => writeFPRD(rd, FP64_Sub(r, FPRD(rs1), FPRD(rs2)))
    case       _,_      => signalException(E_Illegal_Instr)
  }
}

-----------------------------------
-- FMUL.D   rd, rs1, rs2
-----------------------------------

define FArith > FMUL_D(rd::reg, rs1::reg, rs2::reg, fprnd::fprnd) =
{ match round(fprnd), canDoFPDouble()
  { case Some(r), true  => writeFPRD(rd, FP64_Mul(r, FPRD(rs1), FPRD(rs2)))
    case       _,_      => signalException(E_Illegal_Instr)
  }
}

-----------------------------------
-- FDIV.D   rd, rs1, rs2
-----------------------------------

define FArith > FDIV_D(rd::reg, rs1::reg, rs2::reg, fprnd::fprnd) =
{ match round(fprnd), canDoFPDouble()
  { case Some(r), true  => writeFPRD(rd, FP64_Div(r, FPRD(rs1), FPRD(rs2)))
    case       _,_      => signalException(E_Illegal_Instr)
  }
}

-----------------------------------
-- FSQRT.D   rd, rs
-----------------------------------

define FArith > FSQRT_D(rd::reg, rs::reg, fprnd::fprnd) =
{ match round(fprnd), canDoFPDouble()
  { case Some(r), true  => writeFPRD(rd, FP64_Sqrt(r, FPRD(rs)))
    case       _,_      => signalException(E_Illegal_Instr)
  }
}

-----------------------------------
-- FMIN.D    rd, rs1, rs2
-----------------------------------
define FArith > FMIN_D(rd::reg, rs1::reg, rs2::reg) =
{ if   not canDoFPDouble()
  then signalException(E_Illegal_Instr)
  else { f1 = FPRD(rs1)
       ; f2 = FPRD(rs2)
       ; res = match FP64_Compare(f1, f2)
               { case FP_LT => f1
                 case FP_EQ => f1
                 case FP_GT => f2
                 case FP_UN => if (   (FP64_IsSignalingNan(f1) or FP64_IsSignalingNan(f2))
                                   or (f1 == RV64_CanonicalNan and f2 == RV64_CanonicalNan))
                               then RV64_CanonicalNan
                               else -- either f1 or f2 should be a quiet NaN
                                   if f1 == RV64_CanonicalNan then f2 else f1

               }
       ; writeFPRD(rd, res)
       }
}

-----------------------------------
-- FMAX.D    rd, rs1, rs2
-----------------------------------
define FArith > FMAX_D(rd::reg, rs1::reg, rs2::reg) =
{ if   not canDoFPDouble()
  then signalException(E_Illegal_Instr)
  else { f1 = FPRD(rs1)
       ; f2 = FPRD(rs2)
       ; res = match FP64_Compare(f1, f2)
               { case FP_LT => f2
                 case FP_EQ => f2
                 case FP_GT => f1
                 case FP_UN => if (   (FP64_IsSignalingNan(f1) or FP64_IsSignalingNan(f2))
                                   or (f1 == RV64_CanonicalNan and f2 == RV64_CanonicalNan))
                               then RV64_CanonicalNan
                               else -- either f1 or f2 should be a quiet NaN
                                   if f1 == RV64_CanonicalNan then f2 else f1
               }
       ; writeFPRD(rd, res)
       }
}

-----------------------------------
-- FMADD.D   rd, rs1, rs2, rs3
-----------------------------------

define FArith > FMADD_D(rd::reg, rs1::reg, rs2::reg, rs3::reg, fprnd::fprnd) =
{ match round(fprnd), canDoFPDouble()
  { case Some(r), true  => writeFPRD(rd, FP64_Add(r, FP64_Mul(r, FPRD(rs1), FPRD(rs2)), FPRD(rs3)))
    case       _,_      => signalException(E_Illegal_Instr)
  }
}

-----------------------------------
-- FMSUB.D   rd, rs1, rs2, rs3
-----------------------------------

define FArith > FMSUB_D(rd::reg, rs1::reg, rs2::reg, rs3::reg, fprnd::fprnd) =
{ match round(fprnd), canDoFPDouble()
  { case Some(r), true  => writeFPRD(rd, FP64_Sub(r, FP64_Mul(r, FPRD(rs1), FPRD(rs2)), FPRD(rs3)))
    case       _,_      => signalException(E_Illegal_Instr)
  }
}

-----------------------------------
-- FNMADD.D   rd, rs1, rs2, rs3
-----------------------------------

define FArith > FNMADD_D(rd::reg, rs1::reg, rs2::reg, rs3::reg, fprnd::fprnd) =
{ match round(fprnd), canDoFPDouble()
  { case Some(r), true  => writeFPRD(rd, FP64_Neg(FP64_Add(r, FP64_Mul(r, FPRD(rs1), FPRD(rs2)), FPRD(rs3))))
    case       _,_      => signalException(E_Illegal_Instr)
  }
}

-----------------------------------
-- FNMSUB.D   rd, rs1, rs2, rs3
-----------------------------------

define FArith > FNMSUB_D(rd::reg, rs1::reg, rs2::reg, rs3::reg, fprnd::fprnd) =
{ match round(fprnd), canDoFPDouble()
  { case Some(r), true  => writeFPRD(rd, FP64_Neg(FP64_Sub(r, FP64_Mul(r, FPRD(rs1), FPRD(rs2)), FPRD(rs3))))
    case       _,_      => signalException(E_Illegal_Instr)
  }
}

-- Conversions

-----------------------------------
-- FCVT.D.W   rd, rs
-----------------------------------

define FConv > FCVT_D_W(rd::reg, rs::reg, fprnd::fprnd) =
{ match round(fprnd), canDoFPDouble()
  { case Some(r), true  => writeFPRD(rd, FP64_FromInt(r, [GPR(rs)<31:0>]::int))
    case       _,_      => signalException(E_Illegal_Instr)
  }
}

-----------------------------------
-- FCVT.D.WU  rd, rs
-----------------------------------

define FConv > FCVT_D_WU(rd::reg, rs::reg, fprnd::fprnd) =
{ match round(fprnd), canDoFPDouble()
  { case Some(r), true  => writeFPRD(rd, FP64_FromInt(r, [0`1 : GPR(rs)<31:0>]::int))
    case       _,_      => signalException(E_Illegal_Instr)
  }
}

-----------------------------------
-- FCVT.W.D   rd, rs
-----------------------------------

define FConv > FCVT_W_D(rd::reg, rs::reg, fprnd::fprnd) =
{ match round(fprnd), canDoFPDouble()
  { case Some(r), true  => { inp = FPRD(rs)
                           ; val = ValOf(FP64_ToInt(r, inp))
                           ; res = if   FP64_IsNan(inp) or inp == FP64_PosInf
                                   then [0n2 ** 31 - 1]
                                   else if inp == FP64_NegInf
                                   then -[0n2 ** 31]
                                   else if val > 2 ** 31 - 1
                                   then [0n2 ** 31 - 1]
                                   else if val < -2 ** 31
                                   then -[0n2 ** 31]
                                   else [val]
                           ; writeRD(rd, res)
                           }
    case      _,_       => signalException(E_Illegal_Instr)
  }
}

-----------------------------------
-- FCVT.WU.D  rd, rs
-----------------------------------

define FConv > FCVT_WU_D(rd::reg, rs::reg, fprnd::fprnd) =
{ match round(fprnd), canDoFPDouble()
  { case Some(r), true => { inp = FPRD(rs)
                          ; val = ValOf(FP64_ToInt(r, inp))
                          ; res = if   FP64_IsNan(inp) or inp == FP64_PosInf
                                  then [0n2 ** 32 - 1]
                                  else if inp == FP64_NegInf
                                  then 0x0
                                  else if val > 2 ** 32 - 1
                                  then [0n2 ** 32 - 1]
                                  else if val < 0
                                  then 0x0
                                  else [val]
                          ; writeRD(rd, res)
                          }
    case      _,_       => signalException(E_Illegal_Instr)
  }
}

-----------------------------------
-- FCVT.D.L   rd, rs
-----------------------------------

define FConv > FCVT_D_L(rd::reg, rs::reg, fprnd::fprnd) =
{ match round(fprnd), canDoFPDouble()
  { case Some(r), true  => writeFPRD(rd, FP64_FromInt(r, [GPR(rs)]::int))
    case       _,_      => signalException(E_Illegal_Instr)
  }
}

-----------------------------------
-- FCVT.D.LU  rd, rs
-----------------------------------

define FConv > FCVT_D_LU(rd::reg, rs::reg, fprnd::fprnd) =
{ match round(fprnd), canDoFPDouble()
  { case Some(r), true  => writeFPRD(rd, FP64_FromInt(r, [0`1 : GPR(rs)]::int))
    case       _,_      => signalException(E_Illegal_Instr)
  }
}

-----------------------------------
-- FCVT.L.D   rd, rs
-----------------------------------

define FConv > FCVT_L_D(rd::reg, rs::reg, fprnd::fprnd) =
{ match round(fprnd), canDoFPDouble()
  { case Some(r), true => { inp = FPRD(rs)
                          ; val = ValOf(FP64_ToInt(r, inp))
                          ; res = if   FP64_IsNan(inp) or inp == FP64_PosInf
                                  then [0n2 ** 63 - 1]
                                  else if inp == FP64_NegInf
                                  then -[0n2 ** 63]
                                  else if val > 2 ** 63 - 1
                                  then [0n2 ** 63 - 1]
                                  else if val < -2 ** 63
                                  then -[0n2 ** 63]
                                  else [val]
                          ; writeRD(rd, res)
                          }
    case      _,_       => signalException(E_Illegal_Instr)
  }
}

-----------------------------------
-- FCVT.LU.D  rd, rs
-----------------------------------

define FConv > FCVT_LU_D(rd::reg, rs::reg, fprnd::fprnd) =
{ match round(fprnd), canDoFPDouble()
  { case Some(r), true => { inp = FPRD(rs)
                          ; val = ValOf(FP64_ToInt(r, inp))
                          ; res = if   FP64_IsNan(inp) or inp == FP64_PosInf
                                  then [0n2 ** 64 - 1]
                                  else if inp == FP64_NegInf
                                  then 0x0
                                  else if val > 2 ** 64 - 1
                                  then [0n2 ** 64 - 1]
                                  else if val < 0
                                  then 0x0
                                  else [val]
                          ; writeRD(rd, res)
                          }
    case      _,_       => signalException(E_Illegal_Instr)
  }
}

-----------------------------------
-- FCVT.S.D  rd, rs
-----------------------------------

define FConv > FCVT_S_D(rd::reg, rs::reg, fprnd::fprnd) =
{ match round(fprnd), canDoFPDouble()
  { case Some(r), true  => writeFPRS(rd, FP64_ToFP32(r, FPRD(rs)))
    case       _,_      => signalException(E_Illegal_Instr)
  }
}

-----------------------------------
-- FCVT.D.S  rd, rs
-----------------------------------

define FConv > FCVT_D_S(rd::reg, rs::reg, fprnd::fprnd) =
{ match round(fprnd), canDoFPDouble()
  { case Some(r), true  => writeFPRD(rd, FP32_ToFP64(FPRS(rs)))
    case       _,_      => signalException(E_Illegal_Instr)
  }
}

-- Sign injection

-----------------------------------
-- FSGNJ.D  rd, rs
-----------------------------------

define FConv > FSGNJ_D(rd::reg, rs1::reg, rs2::reg) =
{ if   not canDoFPDouble()
  then signalException(E_Illegal_Instr)
  else { f1 = FPRD(rs1)
       ; f2 = FPRD(rs2)
       ; writeFPRD(rd, ([FP64_Sign(f2)]::bits(1)):f1<62:0>)
       }
}

-----------------------------------
-- FSGNJN.D  rd, rs
-----------------------------------

define FConv > FSGNJN_D(rd::reg, rs1::reg, rs2::reg) =
{ if   not canDoFPDouble()
  then signalException(E_Illegal_Instr)
  else { f1 = FPRD(rs1)
       ; f2 = FPRD(rs2)
       ; writeFPRD(rd, ([!FP64_Sign(f2)]::bits(1)):f1<62:0>)
       }
}

-----------------------------------
-- FSGNJX.D  rd, rs
-----------------------------------

define FConv > FSGNJX_D(rd::reg, rs1::reg, rs2::reg) =
{ if   not canDoFPDouble()
  then signalException(E_Illegal_Instr)
  else { f1 = FPRD(rs1)
       ; f2 = FPRD(rs2)
       ; writeFPRD(rd, ([FP64_Sign(f2)]::bits(1) ?? [FP64_Sign(f1)]::bits(1)) : f1<62:0>)
       }
}

-- Movement

-----------------------------------
-- FMV.X.D   rd, rs
-----------------------------------

define FConv > FMV_X_D(rd::reg, rs::reg) =
    if   not canDoFPDouble()
    then signalException(E_Illegal_Instr)
    else writeRD(rd, SignExtend(FPRD(rs)))

-----------------------------------
-- FMV.D.X   rd, rs
-----------------------------------

define FConv > FMV_D_X(rd::reg, rs::reg) =
    if   not canDoFPDouble()
    then signalException(E_Illegal_Instr)
    else writeFPRD(rd, GPR(rs))

-- Comparisons

-----------------------------------
-- FEQ.D   rd, rs
-----------------------------------

define FArith > FEQ_D(rd::reg, rs1::reg, rs2::reg) =
{ if   not canDoFPDouble()
  then signalException(E_Illegal_Instr)
  else { f1  = FPRD(rs1)
       ; f2  = FPRD(rs2)
       ; if FP64_IsSignalingNan(f1) or FP64_IsSignalingNan(f2)
         then { writeRD(rd, 0x0)
              ; setFP_Invalid()
              }
         else { res = match FP64_Compare(f1, f2)
                      { case FP_LT => 0x0
                        case FP_EQ => 0x1
                        case FP_GT => 0x0
                        case FP_UN => 0x0
                      }
              ; writeRD(rd, res)
              }
       }
}

-----------------------------------
-- FLT.D   rd, rs
-----------------------------------

define FArith > FLT_D(rd::reg, rs1::reg, rs2::reg) =
{ if   not canDoFPDouble()
  then signalException(E_Illegal_Instr)
  else { f1  = FPRD(rs1)
       ; f2  = FPRD(rs2)
       ; if   FP64_IsNan(f1) or FP64_IsNan(f2)
         then { writeRD(rd, 0x0)
              ; setFP_Invalid()
              }
         else { res = match FP64_Compare(f1, f2)
                      { case FP_LT => 0x1
                        case FP_EQ => 0x0
                        case FP_GT => 0x0
                        case FP_UN => 0x0
                      }
              ; writeRD(rd, res)
              }
       }
}

-----------------------------------
-- FLE.D   rd, rs
-----------------------------------

define FArith > FLE_D(rd::reg, rs1::reg, rs2::reg) =
{ if   not canDoFPDouble()
  then signalException(E_Illegal_Instr)
  else { f1  = FPRD(rs1)
       ; f2  = FPRD(rs2)
       ; if   FP64_IsNan(f1) or FP64_IsNan(f2)
         then { writeRD(rd, 0x0)
              ; setFP_Invalid()
              }
         else { res = match FP64_Compare(f1, f2)
                      { case FP_LT => 0x1
                        case FP_EQ => 0x1
                        case FP_GT => 0x0
                        case FP_UN => 0x0
                      }
              ; writeRD(rd, res)
              }
       }
}

-- Classification

-----------------------------------
-- FCLASS.D  rd, rs
-----------------------------------

define FConv > FCLASS_D(rd::reg, rs::reg) =
{ if   not canDoFPDouble()
  then signalException(E_Illegal_Instr)
  else { var ret = 0x0`10
       ; val = FPRD(rs)
       ; ret<0> <- val == FP64_NegInf
       ; ret<1> <- FP64_Sign(val) and FP64_IsNormal(val)
       ; ret<2> <- FP64_Sign(val) and FP64_IsSubnormal(val)
       ; ret<3> <- val == FP64_NegZero
       ; ret<4> <- val == FP64_PosZero
       ; ret<5> <- !FP64_Sign(val) and FP64_IsSubnormal(val)
       ; ret<6> <- !FP64_Sign(val) and FP64_IsNormal(val)
       ; ret<7> <- val == FP64_PosInf
       ; ret<8> <- FP64_IsSignalingNan(val)
       ; ret<9> <- val == RV64_CanonicalNan
       ; writeRD(rd, ZeroExtend(ret))
       }
}

---------------------------------------------------------------------------
-- System Instructions
---------------------------------------------------------------------------

-----------------------------------
-- ECALL
-----------------------------------
define System > ECALL  =
    match curPrivilege
    { case User       => signalException(E_U_EnvCall)
      case Supervisor => signalException(E_S_EnvCall)
      case Machine    => signalException(E_M_EnvCall)
    }

-----------------------------------
-- EBREAK
-----------------------------------

define System > EBREAK =
    signalAddressException(E_Breakpoint, PC)

-----------------------------------
-- URET
-----------------------------------
define System > URET   =
    NextFetch <- Some(Uret)

-----------------------------------
-- SRET
-----------------------------------
define System > SRET   =
{ match curPrivilege
  { case Machine    => NextFetch <- Some(Sret)
    case Supervisor => if MCSR.mstatus.M_TSR
                       then signalException(E_Illegal_Instr)
                       else NextFetch <- Some(Sret)
    case User       => signalException(E_Illegal_Instr)
  }
}

-----------------------------------
-- MRET
-----------------------------------
define System > MRET   =
{ match curPrivilege
  { case Machine    => NextFetch <- Some(Mret)
    case Supervisor => signalException(E_Illegal_Instr)
    case User       => signalException(E_Illegal_Instr)
  }
}

-----------------------------------
-- WFI
-----------------------------------
define System > WFI    =
{ match curPrivilege
  { case Machine    => ()
    case Supervisor => if MCSR.mstatus.M_TW
                       then signalException(E_Illegal_Instr)
                       else ()
    case User       => signalException(E_Illegal_Instr)
                       -- TODO: extend to 'N' extension
  }
}

-- Control and Status Registers

bool checkCSROp(csr::imm12, rs1::reg, a::accessType) =
    is_CSR_defined(csr, curPrivilege)
    and check_CSR_access(csrRW(csr), csrPR(csr), curPrivilege, a)
    and check_TVM_SATP(csr, curPrivilege, a)

-----------------------------------
-- CSRRW  rd, rs1, imm
-----------------------------------
define System > CSRRW(rd::reg, rs1::reg, csr::imm12) =
    if checkCSROp(csr, rs1, Write)
    then { val = CSR(csr)
         ; writeCSR(csr, GPR(rs1))
         ; writeRD(rd, val)
         }
    else signalException(E_Illegal_Instr)

-----------------------------------
-- CSRRS  rd, rs1, imm
-----------------------------------
define System > CSRRS(rd::reg, rs1::reg, csr::imm12) =
    if checkCSROp(csr, rs1, if rs1 == 0 then Read else Write)
    then { val = CSR(csr)
         ; when rs1 != 0
           do writeCSR(csr, val || GPR(rs1))
         ; writeRD(rd, val)
         }
    else signalException(E_Illegal_Instr)

-----------------------------------
-- CSRRC  rd, rs1, imm
-----------------------------------
define System > CSRRC(rd::reg, rs1::reg, csr::imm12) =
    if checkCSROp(csr, rs1, if rs1 == 0 then Read else Write)
    then { val = CSR(csr)
         ; when rs1 != 0
           do writeCSR(csr, val && ~GPR(rs1))
         ; writeRD(rd, val)
         }
    else signalException(E_Illegal_Instr)

-----------------------------------
-- CSRRWI rd, rs1, imm
-----------------------------------
define System > CSRRWI(rd::reg, zimm::reg, csr::imm12) =
    if checkCSROp(csr, zimm, if zimm == 0 then Read else Write)
    then { when rd != 0
           do writeRD(rd, CSR(csr))
         ; writeCSR(csr, ZeroExtend(zimm))
         }
    else signalException(E_Illegal_Instr)

-----------------------------------
-- CSRRSI rd, rs1, imm
-----------------------------------
define System > CSRRSI(rd::reg, zimm::reg, csr::imm12) =
    if checkCSROp(csr, zimm, if zimm == 0 then Read else Write)
    then { val = CSR(csr)
         ; when zimm != 0
           do writeCSR(csr, val || ZeroExtend(zimm))
         ; writeRD(rd, val)
         }
    else signalException(E_Illegal_Instr)

-----------------------------------
-- CSRRCI rd, rs1, imm
-----------------------------------
define System > CSRRCI(rd::reg, zimm::reg, csr::imm12) =
    if checkCSROp(csr, zimm, if zimm == 0 then Read else Write)
    then { val = CSR(csr)
         ; when zimm != 0
           do writeCSR(csr, val && ~ZeroExtend(zimm))
         ; writeRD(rd, val)
         }
    else signalException(E_Illegal_Instr)

-- Address translation cache flush

-----------------------------------
-- SFENCE.VM
-----------------------------------
-- TODO: FIXME: update definition
define System > SFENCE_VM(rs1::reg, rs2::reg) =
{ addr = if rs1 == 0 then None else Some(GPR(rs1))
; arch = architecture(MCSR.mstatus.M_SXL)
; mode = match arch
         { case RV32  => if satp32(SCSR.satp<31:0>).SATP32_MODE
                         then Sv32 else Sbare
           case RV64  => satpMode_ofbits(satp64(SCSR.satp).SATP64_MODE, arch)
           case RV128 => -- FIXME: the spec is not clear what happens
                         -- when satp does not contain a sensible value
                         #INTERNAL_ERROR("sfence.vm: undefined satp spec error")
         }
; match mode, MCSR.mstatus.M_TVM
  { case _,     true  => signalException(E_Illegal_Instr)
    case Sbare, false => ()
    case Sv32,  false =>
    { addr = if IsSome(addr) then Some(ValOf(addr)<31:0>) else None
    ; asid = satp32(SCSR.satp<31:0>).SATP32_ASID
    ; TLB32 <- flushTLB32(asid, addr, TLB32)
    }
    case Sv39,  false =>
    { addr = if IsSome(addr) then Some(ValOf(addr)<38:0>) else None
    ; asid = satp64(SCSR.satp).SATP64_ASID
    ; TLB39 <- flushTLB39(asid, addr, TLB39)
    }
    case _,     false =>
    #INTERNAL_ERROR(["sfence.vm: unimplemented VM model " : [mode]::string]) -- FIXME
  }
}

-----------------------------------
-- Unsupported instructions
-----------------------------------
define UnknownInstruction =
    signalException(E_Illegal_Instr)

-----------------------------------
-- Internal pseudo-instructions
-----------------------------------

-- The argument is the value from the PC.

define Internal > FETCH_MISALIGNED(addr::regType) =
{ signalAddressException(E_Fetch_Addr_Align, [addr])
; recordFetchException()
}

define Internal > FETCH_FAULT(addr::regType) =
{ signalAddressException(E_Fetch_Page_Fault, [addr])
; recordFetchException()
}

define Run

--------------------------------------------------
-- Instruction fetch
--------------------------------------------------

construct FetchResult
{ F_Error   :: instruction
, F_Result  :: word
}

FetchResult Fetch() =
{ vPC    = PC
; if vPC<1:0> != 0
  then F_Error(Internal(FETCH_MISALIGNED(vPC)))
  else match translateAddr(vPC, Execute, Instruction)
       { case Some(pPC) => { instw = rawReadInst(pPC)
                           ; recordFetch(instw)
                           ; F_Result(instw)
                           }
         case None      => F_Error(Internal(FETCH_FAULT(vPC)))
       }
}

---------------------------------------------------------------------------
-- Instruction decoding
---------------------------------------------------------------------------

-- helpers to assemble various immediates from their pieces
imm12 asImm12(imm12::bits(1), imm11::bits(1), immhi::bits(6), immlo::bits(4)) =
    imm12 : imm11 : immhi : immlo

imm20 asImm20(imm20::bits(1), immhi::bits(8), imm11::bits(1), immlo::bits(10)) =
    imm20 : immhi : imm11 : immlo

imm12 asSImm12(immhi::bits(7), immlo::bits(5)) =  immhi : immlo

-- decoders organized by major opcode

instruction decode_LOAD(w::word) =
   match w
   { case 'imm           rs1 000  rd 00000 11' => Load( LB(rd, rs1, imm))
     case 'imm           rs1 001  rd 00000 11' => Load( LH(rd, rs1, imm))
     case 'imm           rs1 010  rd 00000 11' => Load( LW(rd, rs1, imm))
     case 'imm           rs1 011  rd 00000 11' => Load( LD(rd, rs1, imm))
     case 'imm           rs1 100  rd 00000 11' => Load(LBU(rd, rs1, imm))
     case 'imm           rs1 101  rd 00000 11' => Load(LHU(rd, rs1, imm))
     case 'imm           rs1 110  rd 00000 11' => Load(LWU(rd, rs1, imm))
     case _                                    => UnknownInstruction
   }

instruction decode_LOAD_FP(w::word) =
   match w
   { case 'imm           rs1 010  rd 00001 11' => FPLoad(FLW(rd, rs1, imm))
     case 'imm           rs1 011  rd 00001 11' => FPLoad(FLD(rd, rs1, imm))
     case _                                    => UnknownInstruction
   }
instruction decode_MISC_MEM(w::word) =
   match w
   { case '_`4 pred succ rs1 000  rd 00011 11' =>   FENCE(rd, rs1, pred, succ)
     case 'imm           rs1 001  rd 00011 11' => FENCE_I(rd, rs1, imm)
     case _                                    => UnknownInstruction
   }

instruction decode_OP_IMM(w::word) =
   match w
   { case 'imm           rs1 000  rd 00100 11' => ArithI( ADDI(rd, rs1, imm))
     case '000000  shamt rs1 001  rd 00100 11' =>  Shift( SLLI(rd, rs1, shamt))
     case 'imm           rs1 010  rd 00100 11' => ArithI( SLTI(rd, rs1, imm))
     case 'imm           rs1 011  rd 00100 11' => ArithI(SLTIU(rd, rs1, imm))
     case 'imm           rs1 100  rd 00100 11' => ArithI( XORI(rd, rs1, imm))
     case '000000  shamt rs1 101  rd 00100 11' =>  Shift( SRLI(rd, rs1, shamt))
     case '010000  shamt rs1 101  rd 00100 11' =>  Shift( SRAI(rd, rs1, shamt))
     case 'imm           rs1 110  rd 00100 11' => ArithI(  ORI(rd, rs1, imm))
     case 'imm           rs1 111  rd 00100 11' => ArithI( ANDI(rd, rs1, imm))
     case _                                    => UnknownInstruction
   }

instruction decode_OP_IMM_32(w::word) =
   match w
   { case 'imm           rs1 000  rd 00110 11' => ArithI(ADDIW(rd, rs1, imm))
     case '0000000 shamt rs1 001  rd 00110 11' =>  Shift(SLLIW(rd, rs1, shamt))
     case '0000000 shamt rs1 101  rd 00110 11' =>  Shift(SRLIW(rd, rs1, shamt))
     case '0100000 shamt rs1 101  rd 00110 11' =>  Shift(SRAIW(rd, rs1, shamt))
     case _                                    => UnknownInstruction
   }

instruction decode_STORE(w::word) =
   match w
   { case 'ihi       rs2 rs1 000 ilo 01000 11' => Store(SB(rs1, rs2, asSImm12(ihi, ilo)))
     case 'ihi       rs2 rs1 001 ilo 01000 11' => Store(SH(rs1, rs2, asSImm12(ihi, ilo)))
     case 'ihi       rs2 rs1 010 ilo 01000 11' => Store(SW(rs1, rs2, asSImm12(ihi, ilo)))
     case 'ihi       rs2 rs1 011 ilo 01000 11' => Store(SD(rs1, rs2, asSImm12(ihi, ilo)))
     case _                                    => UnknownInstruction
   }

instruction decode_STORE_FP(w::word) =
   match w
   { case 'ihi       rs2 rs1 010 ilo 01001 11' => FPStore(FSW(rs1, rs2, asSImm12(ihi, ilo)))
     case 'ihi       rs2 rs1 011 ilo 01001 11' => FPStore(FSD(rs1, rs2, asSImm12(ihi, ilo)))
     case _                                    => UnknownInstruction
   }

instruction decode_AMO(w::word) =
   match w
   { case '00010 aq rl 00000  rs1 010 rd 01011 11' => AMO(     LR_W(aq, rl, rd, rs1))
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

     case _                                        => UnknownInstruction
   }

instruction decode_OP(w::word) =
   match w
   { case '0000000   rs2 rs1 000  rd 01100 11' => ArithR(  ADD(rd, rs1, rs2))
     case '0100000   rs2 rs1 000  rd 01100 11' => ArithR(  SUB(rd, rs1, rs2))
     case '0000000   rs2 rs1 001  rd 01100 11' =>  Shift(  SLL(rd, rs1, rs2))
     case '0000000   rs2 rs1 010  rd 01100 11' => ArithR(  SLT(rd, rs1, rs2))
     case '0000000   rs2 rs1 011  rd 01100 11' => ArithR( SLTU(rd, rs1, rs2))
     case '0000000   rs2 rs1 100  rd 01100 11' => ArithR(  XOR(rd, rs1, rs2))
     case '0000000   rs2 rs1 101  rd 01100 11' =>  Shift(  SRL(rd, rs1, rs2))
     case '0100000   rs2 rs1 101  rd 01100 11' =>  Shift(  SRA(rd, rs1, rs2))
     case '0000000   rs2 rs1 110  rd 01100 11' => ArithR(   OR(rd, rs1, rs2))
     case '0000000   rs2 rs1 111  rd 01100 11' => ArithR(  AND(rd, rs1, rs2))

     case '0000001   rs2 rs1 000  rd 01100 11' => MulDiv(   MUL(rd, rs1, rs2))
     case '0000001   rs2 rs1 001  rd 01100 11' => MulDiv(  MULH(rd, rs1, rs2))
     case '0000001   rs2 rs1 010  rd 01100 11' => MulDiv(MULHSU(rd, rs1, rs2))
     case '0000001   rs2 rs1 011  rd 01100 11' => MulDiv( MULHU(rd, rs1, rs2))
     case '0000001   rs2 rs1 100  rd 01100 11' => MulDiv(   DIV(rd, rs1, rs2))
     case '0000001   rs2 rs1 101  rd 01100 11' => MulDiv(  DIVU(rd, rs1, rs2))
     case '0000001   rs2 rs1 110  rd 01100 11' => MulDiv(   REM(rd, rs1, rs2))
     case '0000001   rs2 rs1 111  rd 01100 11' => MulDiv(  REMU(rd, rs1, rs2))

     case _                                    => UnknownInstruction
   }

instruction decode_OP_32(w::word) =
   match w
   { case '0000000   rs2 rs1 000  rd 01110 11' => ArithR( ADDW(rd, rs1, rs2))
     case '0100000   rs2 rs1 000  rd 01110 11' => ArithR( SUBW(rd, rs1, rs2))
     case '0000000   rs2 rs1 001  rd 01110 11' =>  Shift( SLLW(rd, rs1, rs2))
     case '0000000   rs2 rs1 101  rd 01110 11' =>  Shift( SRLW(rd, rs1, rs2))
     case '0100000   rs2 rs1 101  rd 01110 11' =>  Shift( SRAW(rd, rs1, rs2))

     case '0000001   rs2 rs1 000  rd 01110 11' => MulDiv(  MULW(rd, rs1, rs2))
     case '0000001   rs2 rs1 100  rd 01110 11' => MulDiv(  DIVW(rd, rs1, rs2))
     case '0000001   rs2 rs1 101  rd 01110 11' => MulDiv( DIVUW(rd, rs1, rs2))
     case '0000001   rs2 rs1 110  rd 01110 11' => MulDiv(  REMW(rd, rs1, rs2))
     case '0000001   rs2 rs1 111  rd 01110 11' => MulDiv( REMUW(rd, rs1, rs2))

     case _                                    => UnknownInstruction
   }

instruction decode_MADD(w::word) =
   match w
   { case 'rs3  00   rs2 rs1 frm  rd 10000 11' => FArith(  FMADD_S(rd, rs1, rs2, rs3, frm))
     case 'rs3  01   rs2 rs1 frm  rd 10000 11' => FArith(  FMADD_D(rd, rs1, rs2, rs3, frm))
     case _                                    => UnknownInstruction
   }

instruction decode_MSUB(w::word) =
   match w
   { case 'rs3  00   rs2 rs1 frm  rd 10001 11' => FArith(  FMSUB_S(rd, rs1, rs2, rs3, frm))
     case 'rs3  01   rs2 rs1 frm  rd 10001 11' => FArith(  FMSUB_D(rd, rs1, rs2, rs3, frm))
     case _                                    => UnknownInstruction
   }

instruction decode_NMSUB(w::word) =
   match w
   { case 'rs3  00   rs2 rs1 frm  rd 10010 11' => FArith( FNMSUB_S(rd, rs1, rs2, rs3, frm))
     case 'rs3  01   rs2 rs1 frm  rd 10010 11' => FArith( FNMSUB_D(rd, rs1, rs2, rs3, frm))
     case _                                    => UnknownInstruction
   }

instruction decode_NMADD(w::word) =
   match w
   { case 'rs3  00   rs2 rs1 frm  rd 10011 11' => FArith( FNMADD_S(rd, rs1, rs2, rs3, frm))
     case 'rs3  01   rs2 rs1 frm  rd 10011 11' => FArith( FNMADD_D(rd, rs1, rs2, rs3, frm))
     case _                                    => UnknownInstruction
   }

instruction decode_OP_FP(w::word) =
   match w
   { case '0000000   rs2 rs1 frm  rd 10100 11' => FArith(   FADD_S(rd, rs1, rs2, frm))
     case '0000100   rs2 rs1 frm  rd 10100 11' => FArith(   FSUB_S(rd, rs1, rs2, frm))
     case '0001000   rs2 rs1 frm  rd 10100 11' => FArith(   FMUL_S(rd, rs1, rs2, frm))
     case '0001100   rs2 rs1 frm  rd 10100 11' => FArith(   FDIV_S(rd, rs1, rs2, frm))
     case '0101100 00000 rs1 frm  rd 10100 11' => FArith(  FSQRT_S(rd, rs1, frm))

     case '0010100   rs2 rs1 000  rd 10100 11' => FArith(  FMIN_S(rd,  rs1, rs2))
     case '0010100   rs2 rs1 001  rd 10100 11' => FArith(  FMAX_S(rd,  rs1, rs2))
     case '1010000   rs2 rs1 010  rd 10100 11' => FArith(   FEQ_S(rd,  rs1, rs2))
     case '1010000   rs2 rs1 001  rd 10100 11' => FArith(   FLT_S(rd,  rs1, rs2))
     case '1010000   rs2 rs1 000  rd 10100 11' => FArith(   FLE_S(rd,  rs1, rs2))

     case '0010000   rs2 rs1 000  rd 10100 11' => FConv (  FSGNJ_S(rd,  rs1, rs2))
     case '0010000   rs2 rs1 001  rd 10100 11' => FConv ( FSGNJN_S(rd,  rs1, rs2))
     case '0010000   rs2 rs1 010  rd 10100 11' => FConv ( FSGNJX_S(rd,  rs1, rs2))

     case '1100000 00000 rs1 frm  rd 10100 11' => FConv(  FCVT_W_S(rd, rs1, frm))
     case '1100000 00001 rs1 frm  rd 10100 11' => FConv( FCVT_WU_S(rd, rs1, frm))
     case '1110000 00000 rs1 000  rd 10100 11' => FConv(   FMV_X_S(rd, rs1))
     case '1110000 00000 rs1 001  rd 10100 11' => FConv(  FCLASS_S(rd, rs1))
     case '1101000 00000 rs1 frm  rd 10100 11' => FConv(  FCVT_S_W(rd, rs1, frm))
     case '1101000 00001 rs1 frm  rd 10100 11' => FConv( FCVT_S_WU(rd, rs1, frm))
     case '1111000 00000 rs1 000  rd 10100 11' => FConv(   FMV_S_X(rd, rs1))

     case '0000001   rs2 rs1 frm  rd 10100 11' => FArith(   FADD_D(rd, rs1, rs2, frm))
     case '0000101   rs2 rs1 frm  rd 10100 11' => FArith(   FSUB_D(rd, rs1, rs2, frm))
     case '0001001   rs2 rs1 frm  rd 10100 11' => FArith(   FMUL_D(rd, rs1, rs2, frm))
     case '0001101   rs2 rs1 frm  rd 10100 11' => FArith(   FDIV_D(rd, rs1, rs2, frm))
     case '0101101 00000 rs1 frm  rd 10100 11' => FArith(  FSQRT_D(rd, rs1, frm))

     case '0010101   rs2 rs1 000  rd 10100 11' => FArith(  FMIN_D(rd,  rs1, rs2))
     case '0010101   rs2 rs1 001  rd 10100 11' => FArith(  FMAX_D(rd,  rs1, rs2))
     case '1010001   rs2 rs1 010  rd 10100 11' => FArith(   FEQ_D(rd,  rs1, rs2))
     case '1010001   rs2 rs1 001  rd 10100 11' => FArith(   FLT_D(rd,  rs1, rs2))
     case '1010001   rs2 rs1 000  rd 10100 11' => FArith(   FLE_D(rd,  rs1, rs2))

     case '0010001   rs2 rs1 000  rd 10100 11' => FConv (  FSGNJ_D(rd,  rs1, rs2))
     case '0010001   rs2 rs1 001  rd 10100 11' => FConv ( FSGNJN_D(rd,  rs1, rs2))
     case '0010001   rs2 rs1 010  rd 10100 11' => FConv ( FSGNJX_D(rd,  rs1, rs2))

     case '1100001 00000 rs1 frm  rd 10100 11' => FConv(  FCVT_W_D(rd, rs1, frm))
     case '1100001 00001 rs1 frm  rd 10100 11' => FConv( FCVT_WU_D(rd, rs1, frm))
     case '1110001 00000 rs1 001  rd 10100 11' => FConv(  FCLASS_D(rd, rs1))
     case '1101001 00000 rs1 frm  rd 10100 11' => FConv(  FCVT_D_W(rd, rs1, frm))
     case '1101001 00001 rs1 frm  rd 10100 11' => FConv( FCVT_D_WU(rd, rs1, frm))

     case '1100000 00010 rs1 frm  rd 10100 11' => FConv(  FCVT_L_S(rd, rs1, frm))
     case '1100000 00011 rs1 frm  rd 10100 11' => FConv( FCVT_LU_S(rd, rs1, frm))
     case '1101000 00010 rs1 frm  rd 10100 11' => FConv(  FCVT_S_L(rd, rs1, frm))
     case '1101000 00011 rs1 frm  rd 10100 11' => FConv( FCVT_S_LU(rd, rs1, frm))

     case '1100001 00010 rs1 frm  rd 10100 11' => FConv(  FCVT_L_D(rd, rs1, frm))
     case '1100001 00011 rs1 frm  rd 10100 11' => FConv( FCVT_LU_D(rd, rs1, frm))
     case '1101001 00010 rs1 frm  rd 10100 11' => FConv(  FCVT_D_L(rd, rs1, frm))
     case '1101001 00011 rs1 frm  rd 10100 11' => FConv( FCVT_D_LU(rd, rs1, frm))
     case '1110001 00000 rs1 000  rd 10100 11' => FConv(   FMV_X_D(rd, rs1))
     case '1111001 00000 rs1 000  rd 10100 11' => FConv(   FMV_D_X(rd, rs1))

     case '0100000 00001 rs1 frm  rd 10100 11' => FConv(  FCVT_S_D(rd, rs1, frm))
     case '0100001 00000 rs1 frm  rd 10100 11' => FConv(  FCVT_D_S(rd, rs1, frm))

     case _                                    => UnknownInstruction
   }

instruction decode_BRANCH(w::word) =
   match w
   { case 'i12 ihi rs2 rs1 000 ilo i11 11000 11' => Branch( BEQ(rs1, rs2, asImm12(i12, i11, ihi, ilo)))
     case 'i12 ihi rs2 rs1 001 ilo i11 11000 11' => Branch( BNE(rs1, rs2, asImm12(i12, i11, ihi, ilo)))
     case 'i12 ihi rs2 rs1 100 ilo i11 11000 11' => Branch( BLT(rs1, rs2, asImm12(i12, i11, ihi, ilo)))
     case 'i12 ihi rs2 rs1 101 ilo i11 11000 11' => Branch( BGE(rs1, rs2, asImm12(i12, i11, ihi, ilo)))
     case 'i12 ihi rs2 rs1 110 ilo i11 11000 11' => Branch(BLTU(rs1, rs2, asImm12(i12, i11, ihi, ilo)))
     case 'i12 ihi rs2 rs1 111 ilo i11 11000 11' => Branch(BGEU(rs1, rs2, asImm12(i12, i11, ihi, ilo)))
     case _                                      => UnknownInstruction
   }

instruction decode_SYSTEM(w::word) =
   match w
   { case 'csr                rs1 001 rd 11100 11' => System( CSRRW(rd, rs1, csr))
     case 'csr                rs1 010 rd 11100 11' => System( CSRRS(rd, rs1, csr))
     case 'csr                rs1 011 rd 11100 11' => System( CSRRC(rd, rs1, csr))
     case 'csr                imm 101 rd 11100 11' => System(CSRRWI(rd, imm, csr))
     case 'csr                imm 110 rd 11100 11' => System(CSRRSI(rd, imm, csr))
     case 'csr                imm 111 rd 11100 11' => System(CSRRCI(rd, imm, csr))

     case '000000000000  00000 000 00000 11100 11' => System( ECALL)
     case '000000000001  00000 000 00000 11100 11' => System(EBREAK)

     case '000000000010  00000 000 00000 11100 11' => System(  URET)
     case '000100000010  00000 000 00000 11100 11' => System(  SRET)
     case '001100000010  00000 000 00000 11100 11' => System(  MRET)

     case '000100000101  00000 000 00000 11100 11' => System(   WFI)

     case '0001001  rs2    rs1 000 00000 11100 11' => System(SFENCE_VM(rs1, rs2))

     case _                                        => UnknownInstruction
   }

-- decode by major opcode, except in cases where it has a single instruction
instruction Decode(w::word) =
   match w
   { case 'imm           rs1 000  rd 11001 11' => Branch( JALR(rd, rs1, imm))
     case 'i20 ilo i11 ihi        rd 11011 11' => Branch(  JAL(rd, asImm20(i20, ihi, i11, ilo)))

     case 'imm                    rd 00101 11' => ArithI(AUIPC(rd, imm))
     case 'imm                    rd 01101 11' => ArithI(  LUI(rd, imm))

     case '_`25                      00000 11' => decode_LOAD(w)
     case '_`25                      00001 11' => decode_LOAD_FP(w)
     case '_`25                      00011 11' => decode_MISC_MEM(w)
     case '_`25                      00100 11' => decode_OP_IMM(w)
     case '_`25                      00110 11' => decode_OP_IMM_32(w)

     case '_`25                      01000 11' => decode_STORE(w)
     case '_`25                      01001 11' => decode_STORE_FP(w)
     case '_`25                      01011 11' => decode_AMO(w)
     case '_`25                      01100 11' => decode_OP(w)
     case '_`25                      01110 11' => decode_OP_32(w)

     case '_`25                      10000 11' => decode_MADD(w)
     case '_`25                      10001 11' => decode_MSUB(w)
     case '_`25                      10010 11' => decode_NMSUB(w)
     case '_`25                      10011 11' => decode_NMADD(w)
     case '_`25                      10100 11' => decode_OP_FP(w)

     case '_`25                      11000 11' => decode_BRANCH(w)
     case '_`25                      11100 11' => decode_SYSTEM(w)

     case _                                    => UnknownInstruction
   }

-- instruction printer

string imm(i::bits(N))  = "0x" : [i]
string pinst(o::string) = PadRight(#" ", 12, o)

string amotype(aq::amo, rl::amo) =
    match aq, rl
    { case 0, 0 => ""
      case 1, 0 => ".aq"
      case 0, 1 => ".rl"
      case 1, 1 => ".sc"
    }

string pRtype(o::string, rd::reg, rs1::reg, rs2::reg) =
    pinst(o) : " " : reg(rd) : ", " : reg(rs1) : ", " : reg(rs2)

string pARtype(o::string, aq::amo, rl::amo, rd::reg, rs1::reg, rs2::reg) =
    pRtype([o : amotype(aq, rl)], rd, rs1, rs2)

string pLRtype(o::string, aq::amo, rl::amo, rd::reg, rs1::reg) =
    pinst([o : amotype(aq, rl)]) : " " : reg(rd) : ", " : reg(rs1)

string pItype(o::string, rd::reg, rs1::reg, i::bits(N)) =
    pinst(o) : " " : reg(rd) : ", " : reg(rs1) : ", " : imm(i)

string pCSRtype(o::string, rd::reg, rs1::reg, csr::csreg) =
    pinst(o) : " " : reg(rd) : ", " : reg(rs1) : ", " : csrName(csr)

string pCSRItype(o::string, rd::reg, i::bits(N), csr::csreg) =
    pinst(o) : " " : reg(rd) : ", " : imm(i) : ", " : csrName(csr)

string pStype(o::string, rs1::reg, rs2::reg, i::bits(N)) =
    pinst(o) : " " : reg(rs2) : ", " : reg(rs1) : ", " : imm(i)

string pSBtype(o::string, rs1::reg, rs2::reg, i::bits(N)) =
    pinst(o) : " " : reg(rs1) : ", " : reg(rs2) : ", " : imm(i<<1)

string pUtype(o::string, rd::reg, i::bits(N)) =
    pinst(o) : " " : reg(rd) : ", " : imm(i)

string pUJtype(o::string, rd::reg, i::bits(N)) =
    pinst(o) : " " : reg(rd) : ", " : imm(i<<1)

string pN0type(o::string) =
    pinst(o)

string pN1type(o::string, r::reg) =
    pinst(o) : " " : reg(r)

string pFRtype(o::string, rd::reg, rs1::reg, rs2::reg) =
    pinst(o) : " " : fpreg(rd) : ", " : fpreg(rs1) : ", " : fpreg(rs2)

string pFR1type(o::string, rd::reg, rs::reg) =
    pinst(o) : " " : fpreg(rd) : ", " : fpreg(rs)

string pFR3type(o::string, rd::reg, rs1::reg, rs2::reg, rs3::reg) =
    pinst(o) : " " : fpreg(rd) : ", " : fpreg(rs1) : ", " : fpreg(rs2) : ", " : fpreg(rs3)

string pFItype(o::string, rd::reg, rs1::reg, i::bits(N)) =
    pinst(o) : " " : fpreg(rd) : ", " : reg(rs1) : ", " : imm(i)

string pFStype(o::string, rs1::reg, rs2::reg, i::bits(N)) =
    pinst(o) : " " : fpreg(rs2) : ", " : reg(rs1) : ", " : imm(i)

string pCFItype(o::string, rd::reg, rs::reg) =
    pinst(o) : " " : fpreg(rd) : ", " : reg(rs)

string pCIFtype(o::string, rd::reg, rs::reg) =
    pinst(o) : " " : reg(rd) : ", " : fpreg(rs)

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

     case FArith(  FADD_S(rd, rs1, rs2, frm)) => pFRtype("FADD.S", rd, rs1, rs2)
     case FArith(  FSUB_S(rd, rs1, rs2, frm)) => pFRtype("FSUB.S", rd, rs1, rs2)
     case FArith(  FMUL_S(rd, rs1, rs2, frm)) => pFRtype("FMUL.S", rd, rs1, rs2)
     case FArith(  FDIV_S(rd, rs1, rs2, frm)) => pFRtype("FDIV.S", rd, rs1, rs2)

     case FArith( FSQRT_S(rd, rs, frm))       => pFR1type("FSQRT.S", rd, rs)

     case FArith(  FMIN_S(rd, rs1, rs2))      => pFRtype("FMIN.S", rd, rs1, rs2)
     case FArith(  FMAX_S(rd, rs1, rs2))      => pFRtype("FMAX.S", rd, rs1, rs2)
     case FArith(   FEQ_S(rd, rs1, rs2))      => pFRtype("FEQ.S",  rd, rs1, rs2)
     case FArith(   FLT_S(rd, rs1, rs2))      => pFRtype("FLT.S",  rd, rs1, rs2)
     case FArith(   FLE_S(rd, rs1, rs2))      => pFRtype("FLE.S",  rd, rs1, rs2)

     case FArith( FMADD_S(rd, rs1, rs2, rs3, frm)) => pFR3type("FMADD.S",  rd, rs1, rs2, rs3)
     case FArith( FMSUB_S(rd, rs1, rs2, rs3, frm)) => pFR3type("FMSUB.S",  rd, rs1, rs2, rs3)
     case FArith(FNMADD_S(rd, rs1, rs2, rs3, frm)) => pFR3type("FNMADD.S", rd, rs1, rs2, rs3)
     case FArith(FNMSUB_S(rd, rs1, rs2, rs3, frm)) => pFR3type("FNMSUB.S", rd, rs1, rs2, rs3)

     case FArith(  FADD_D(rd, rs1, rs2, frm)) => pFRtype("FADD.D", rd, rs1, rs2)
     case FArith(  FSUB_D(rd, rs1, rs2, frm)) => pFRtype("FSUB.D", rd, rs1, rs2)
     case FArith(  FMUL_D(rd, rs1, rs2, frm)) => pFRtype("FMUL.D", rd, rs1, rs2)
     case FArith(  FDIV_D(rd, rs1, rs2, frm)) => pFRtype("FDIV.D", rd, rs1, rs2)

     case FArith( FSQRT_D(rd, rs, frm))       => pFR1type("FSQRT.D", rd, rs)

     case FArith(  FMIN_D(rd, rs1, rs2))      => pFRtype("FMIN.D", rd, rs1, rs2)
     case FArith(  FMAX_D(rd, rs1, rs2))      => pFRtype("FMAX.D", rd, rs1, rs2)
     case FArith(   FEQ_D(rd, rs1, rs2))      => pFRtype("FEQ.D",  rd, rs1, rs2)
     case FArith(   FLT_D(rd, rs1, rs2))      => pFRtype("FLT.D",  rd, rs1, rs2)
     case FArith(   FLE_D(rd, rs1, rs2))      => pFRtype("FLE.D",  rd, rs1, rs2)

     case FArith( FMADD_D(rd, rs1, rs2, rs3, frm)) => pFR3type("FMADD.D",  rd, rs1, rs2, rs3)
     case FArith( FMSUB_D(rd, rs1, rs2, rs3, frm)) => pFR3type("FMSUB.D",  rd, rs1, rs2, rs3)
     case FArith(FNMADD_D(rd, rs1, rs2, rs3, frm)) => pFR3type("FNMADD.D", rd, rs1, rs2, rs3)
     case FArith(FNMSUB_D(rd, rs1, rs2, rs3, frm)) => pFR3type("FNMSUB.D", rd, rs1, rs2, rs3)

     case FConv(  FSGNJ_S(rd, rs1, rs2))      => pFRtype("FSGNJ.S",    rd, rs1, rs2)
     case FConv( FSGNJN_S(rd, rs1, rs2))      => pFRtype("FSGNJN.S",   rd, rs1, rs2)
     case FConv( FSGNJX_S(rd, rs1, rs2))      => pFRtype("FSGNJX.S",   rd, rs1, rs2)

     case FConv( FCVT_W_S(rd, rs, frm))       => pCIFtype("FCVT.W.S",  rd, rs)
     case FConv(FCVT_WU_S(rd, rs, frm))       => pCIFtype("FCVT.WU.S", rd, rs)
     case FConv(  FMV_X_S(rd, rs))            => pCIFtype("FMV.X.S",   rd, rs)
     case FConv( FCLASS_S(rd, rs))            => pCIFtype("FCLASS.S",  rd, rs)
     case FConv( FCVT_S_W(rd, rs, frm))       => pCFItype("FCVT.S.W",  rd, rs)
     case FConv(FCVT_S_WU(rd, rs, frm))       => pCFItype("FCVT.S.WU", rd, rs)
     case FConv(  FMV_S_X(rd, rs))            => pCFItype("FMV.S.X",   rd, rs)

     case FConv(  FSGNJ_D(rd, rs1, rs2))      => pFRtype("FSGNJ.D",    rd, rs1, rs2)
     case FConv( FSGNJN_D(rd, rs1, rs2))      => pFRtype("FSGNJN.D",   rd, rs1, rs2)
     case FConv( FSGNJX_D(rd, rs1, rs2))      => pFRtype("FSGNJX.D",   rd, rs1, rs2)

     case FConv( FCVT_W_D(rd, rs, frm))       => pCIFtype("FCVT.W.D",  rd, rs)
     case FConv(FCVT_WU_D(rd, rs, frm))       => pCIFtype("FCVT.WU.D", rd, rs)
     case FConv( FCLASS_D(rd, rs))            => pCIFtype("FCLASS.D",  rd, rs)
     case FConv( FCVT_D_W(rd, rs, frm))       => pCFItype("FCVT.D.W",  rd, rs)
     case FConv(FCVT_D_WU(rd, rs, frm))       => pCFItype("FCVT.D.WU", rd, rs)

     case FConv( FCVT_L_S(rd, rs, frm))       => pCIFtype("FCVT.L.S",  rd, rs)
     case FConv(FCVT_LU_S(rd, rs, frm))       => pCIFtype("FCVT.LU.S", rd, rs)
     case FConv( FCVT_S_L(rd, rs, frm))       => pCFItype("FCVT.S.L",  rd, rs)
     case FConv(FCVT_S_LU(rd, rs, frm))       => pCFItype("FCVT.S.LU", rd, rs)
     case FConv( FCVT_L_D(rd, rs, frm))       => pCIFtype("FCVT.L.D",  rd, rs)
     case FConv(FCVT_LU_D(rd, rs, frm))       => pCIFtype("FCVT.LU.D", rd, rs)
     case FConv(  FMV_X_D(rd, rs))            => pCIFtype("FMV.X.D",   rd, rs)
     case FConv( FCVT_D_L(rd, rs, frm))       => pCFItype("FCVT.D.L",  rd, rs)
     case FConv(FCVT_D_LU(rd, rs, frm))       => pCFItype("FCVT.D.LU", rd, rs)
     case FConv(  FMV_D_X(rd, rs))            => pCFItype("FMV.D.X",   rd, rs)
     case FConv( FCVT_D_S(rd, rs, frm))       => pCFItype("FCVT.D.S",  rd, rs)
     case FConv( FCVT_S_D(rd, rs, frm))       => pCFItype("FCVT.S.D",  rd, rs)

     case FPLoad(  FLW(rd, rs1, imm))         => pFItype("FLW",    rd, rs1, imm)
     case FPLoad(  FLD(rd, rs1, imm))         => pFItype("FLD",    rd, rs1, imm)
     case FPStore( FSW(rs1, rs2, imm))        => pFStype("FSW",   rs1, rs2, imm)
     case FPStore( FSD(rs1, rs2, imm))        => pFStype("FSD",   rs1, rs2, imm)

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
     case System(  URET)                    => pN0type("URET")
     case System(  SRET)                    => pN0type("SRET")
     case System(  MRET)                    => pN0type("MRET")

     case System(   WFI)                    => pN0type("WFI")

     case System( CSRRW(rd, rs1, csr))      => pCSRtype("CSRRW",  rd, rs1, csr)
     case System( CSRRS(rd, rs1, csr))      => pCSRtype("CSRRS",  rd, rs1, csr)
     case System( CSRRC(rd, rs1, csr))      => pCSRtype("CSRRC",  rd, rs1, csr)
     case System(CSRRWI(rd, imm, csr))      => pCSRItype("CSRRWI", rd, imm, csr)
     case System(CSRRSI(rd, imm, csr))      => pCSRItype("CSRRSI", rd, imm, csr)
     case System(CSRRCI(rd, imm, csr))      => pCSRItype("CSRRCI", rd, imm, csr)

     case System(SFENCE_VM(rs1, rs2))       => pRtype("SFENCE.VM", 0b0`5, rs1, rs2)

     case UnknownInstruction                => pN0type("UNKNOWN")

     case Internal(FETCH_MISALIGNED(_))     => pN0type("FETCH_MISALIGNED")
     case Internal(FETCH_FAULT(_))          => pN0type("FETCH_FAULT")
   }


word Rtype(o::opcode, f3::bits(3), rd::reg, rs1::reg, rs2::reg, f7::bits(7)) =
    f7 : rs2 : rs1 : f3 : rd : o

word R4type(o::opcode, f3::bits(3), rd::reg, rs1::reg, rs2::reg, rs3::reg, f2::bits(2)) =
    rs3 : f2 : rs2 : rs1 : f3 : rd : o

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
     case  Shift( SLLI(rd, rs1, imm))       =>  Itype(opc(0x04), 1, rd, rs1, '000000' : imm)
     case ArithI( SLTI(rd, rs1, imm))       =>  Itype(opc(0x04), 2, rd, rs1, imm)
     case ArithI(SLTIU(rd, rs1, imm))       =>  Itype(opc(0x04), 3, rd, rs1, imm)
     case ArithI( XORI(rd, rs1, imm))       =>  Itype(opc(0x04), 4, rd, rs1, imm)
     case  Shift( SRLI(rd, rs1, imm))       =>  Itype(opc(0x04), 5, rd, rs1, '000000' : imm)
     case  Shift( SRAI(rd, rs1, imm))       =>  Itype(opc(0x04), 5, rd, rs1, '010000' : imm)
     case ArithI(  ORI(rd, rs1, imm))       =>  Itype(opc(0x04), 6, rd, rs1, imm)
     case ArithI( ANDI(rd, rs1, imm))       =>  Itype(opc(0x04), 7, rd, rs1, imm)

     case ArithR(  ADD(rd, rs1, rs2))       =>  Rtype(opc(0x0C), 0, rd, rs1, rs2, 0)
     case ArithR(  SUB(rd, rs1, rs2))       =>  Rtype(opc(0x0C), 0, rd, rs1, rs2, 0x20)
     case  Shift(  SLL(rd, rs1, rs2))       =>  Rtype(opc(0x0C), 1, rd, rs1, rs2, 0)
     case ArithR(  SLT(rd, rs1, rs2))       =>  Rtype(opc(0x0C), 2, rd, rs1, rs2, 0)
     case ArithR( SLTU(rd, rs1, rs2))       =>  Rtype(opc(0x0C), 3, rd, rs1, rs2, 0)
     case ArithR(  XOR(rd, rs1, rs2))       =>  Rtype(opc(0x0C), 4, rd, rs1, rs2, 0)
     case  Shift(  SRL(rd, rs1, rs2))       =>  Rtype(opc(0x0C), 5, rd, rs1, rs2, 0)
     case  Shift(  SRA(rd, rs1, rs2))       =>  Rtype(opc(0x0C), 5, rd, rs1, rs2, 0x20)
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

     case FArith(  FADD_S(rd, rs1, rs2, frm)) => Rtype(opc(0x14), frm, rd, rs1, rs2, 0x00)
     case FArith(  FSUB_S(rd, rs1, rs2, frm)) => Rtype(opc(0x14), frm, rd, rs1, rs2, 0x04)
     case FArith(  FMUL_S(rd, rs1, rs2, frm)) => Rtype(opc(0x14), frm, rd, rs1, rs2, 0x08)
     case FArith(  FDIV_S(rd, rs1, rs2, frm)) => Rtype(opc(0x14), frm, rd, rs1, rs2, 0x0c)
     case FArith( FSQRT_S(rd, rs, frm))       => Rtype(opc(0x14), frm, rd, rs,    0, 0x2c)
     case FArith(  FMIN_S(rd, rs1, rs2))      => Rtype(opc(0x14), 0,   rd, rs1, rs2, 0x14)
     case FArith(  FMAX_S(rd, rs1, rs2))      => Rtype(opc(0x14), 1,   rd, rs1, rs2, 0x14)
     case FArith(   FEQ_S(rd, rs1, rs2))      => Rtype(opc(0x14), 2,   rd, rs1, rs2, 0x50)
     case FArith(   FLT_S(rd, rs1, rs2))      => Rtype(opc(0x14), 1,   rd, rs1, rs2, 0x50)
     case FArith(   FLE_S(rd, rs1, rs2))      => Rtype(opc(0x14), 0,   rd, rs1, rs2, 0x50)

     case FArith(  FADD_D(rd, rs1, rs2, frm)) => Rtype(opc(0x14), frm, rd, rs1, rs2, 0x01)
     case FArith(  FSUB_D(rd, rs1, rs2, frm)) => Rtype(opc(0x14), frm, rd, rs1, rs2, 0x05)
     case FArith(  FMUL_D(rd, rs1, rs2, frm)) => Rtype(opc(0x14), frm, rd, rs1, rs2, 0x09)
     case FArith(  FDIV_D(rd, rs1, rs2, frm)) => Rtype(opc(0x14), frm, rd, rs1, rs2, 0x0d)
     case FArith( FSQRT_D(rd, rs, frm))       => Rtype(opc(0x14), frm, rd, rs,    0, 0x2d)
     case FArith(  FMIN_D(rd, rs1, rs2))      => Rtype(opc(0x14), 0,   rd, rs1, rs2, 0x15)
     case FArith(  FMAX_D(rd, rs1, rs2))      => Rtype(opc(0x14), 1,   rd, rs1, rs2, 0x15)
     case FArith(   FEQ_D(rd, rs1, rs2))      => Rtype(opc(0x14), 2,   rd, rs1, rs2, 0x51)
     case FArith(   FLT_D(rd, rs1, rs2))      => Rtype(opc(0x14), 1,   rd, rs1, rs2, 0x51)
     case FArith(   FLE_D(rd, rs1, rs2))      => Rtype(opc(0x14), 0,   rd, rs1, rs2, 0x51)

     case FPLoad(  FLW(rd, rs1, imm))         => Itype(opc(0x01), 2, rd, rs1, imm)
     case FPLoad(  FLD(rd, rs1, imm))         => Itype(opc(0x01), 3, rd, rs1, imm)
     case FPStore( FSW(rs1, rs2, imm))        => Stype(opc(0x09), 2, rs1, rs2, imm)
     case FPStore( FSD(rs1, rs2, imm))        => Stype(opc(0x09), 3, rs1, rs2, imm)

     case FArith( FMADD_S(rd, rs1, rs2, rs3, frm)) => R4type(opc(0x10), frm, rd, rs1, rs2, rs3, 0)
     case FArith( FMSUB_S(rd, rs1, rs2, rs3, frm)) => R4type(opc(0x11), frm, rd, rs1, rs2, rs3, 0)
     case FArith(FNMSUB_S(rd, rs1, rs2, rs3, frm)) => R4type(opc(0x12), frm, rd, rs1, rs2, rs3, 0)
     case FArith(FNMADD_S(rd, rs1, rs2, rs3, frm)) => R4type(opc(0x13), frm, rd, rs1, rs2, rs3, 0)

     case FArith( FMADD_D(rd, rs1, rs2, rs3, frm)) => R4type(opc(0x10), frm, rd, rs1, rs2, rs3, 1)
     case FArith( FMSUB_D(rd, rs1, rs2, rs3, frm)) => R4type(opc(0x11), frm, rd, rs1, rs2, rs3, 1)
     case FArith(FNMSUB_D(rd, rs1, rs2, rs3, frm)) => R4type(opc(0x12), frm, rd, rs1, rs2, rs3, 1)
     case FArith(FNMADD_D(rd, rs1, rs2, rs3, frm)) => R4type(opc(0x13), frm, rd, rs1, rs2, rs3, 1)

     case FConv(  FSGNJ_S(rd, rs1, rs2))      => Rtype(opc(0x14), 0, rd, rs1, rs2, 0x10)
     case FConv( FSGNJN_S(rd, rs1, rs2))      => Rtype(opc(0x14), 1, rd, rs1, rs2, 0x10)
     case FConv( FSGNJX_S(rd, rs1, rs2))      => Rtype(opc(0x14), 2, rd, rs1, rs2, 0x10)

     case FConv( FCVT_W_S(rd, rs, frm))       => Rtype(opc(0x14), frm, rd, rs, 0, 0x60)
     case FConv(FCVT_WU_S(rd, rs, frm))       => Rtype(opc(0x14), frm, rd, rs, 1, 0x60)
     case FConv(  FMV_X_S(rd, rs))            => Rtype(opc(0x14), 0,   rd, rs, 0, 0x70)
     case FConv( FCLASS_S(rd, rs))            => Rtype(opc(0x14), 1,   rd, rs, 0, 0x70)
     case FConv( FCVT_S_W(rd, rs, frm))       => Rtype(opc(0x14), frm, rd, rs, 0, 0x68)
     case FConv(FCVT_S_WU(rd, rs, frm))       => Rtype(opc(0x14), frm, rd, rs, 1, 0x68)
     case FConv(  FMV_S_X(rd, rs))            => Rtype(opc(0x14), 0,   rd, rs, 0, 0x78)

     case FConv(  FSGNJ_D(rd, rs1, rs2))      => Rtype(opc(0x14), 0, rd, rs1, rs2, 0x11)
     case FConv( FSGNJN_D(rd, rs1, rs2))      => Rtype(opc(0x14), 1, rd, rs1, rs2, 0x11)
     case FConv( FSGNJX_D(rd, rs1, rs2))      => Rtype(opc(0x14), 2, rd, rs1, rs2, 0x11)

     case FConv( FCVT_W_D(rd, rs, frm))       => Rtype(opc(0x14), frm, rd, rs, 0, 0x61)
     case FConv(FCVT_WU_D(rd, rs, frm))       => Rtype(opc(0x14), frm, rd, rs, 1, 0x61)
     case FConv( FCLASS_D(rd, rs))            => Rtype(opc(0x14), 1,   rd, rs, 0, 0x71)
     case FConv( FCVT_D_W(rd, rs, frm))       => Rtype(opc(0x14), frm, rd, rs, 0, 0x69)
     case FConv(FCVT_D_WU(rd, rs, frm))       => Rtype(opc(0x14), frm, rd, rs, 1, 0x69)
     case FConv( FCVT_S_D(rd, rs, frm))       => Rtype(opc(0x14), frm, rd, rs, 1, 0x20)
     case FConv( FCVT_D_S(rd, rs, frm))       => Rtype(opc(0x14), frm, rd, rs, 0, 0x21)

     case FConv( FCVT_L_S(rd, rs, frm))       => Rtype(opc(0x14), frm, rd, rs, 2, 0x60)
     case FConv(FCVT_LU_S(rd, rs, frm))       => Rtype(opc(0x14), frm, rd, rs, 3, 0x60)
     case FConv( FCVT_S_L(rd, rs, frm))       => Rtype(opc(0x14), frm, rd, rs, 2, 0x68)
     case FConv(FCVT_S_LU(rd, rs, frm))       => Rtype(opc(0x14), frm, rd, rs, 3, 0x68)
     case FConv( FCVT_L_D(rd, rs, frm))       => Rtype(opc(0x14), frm, rd, rs, 2, 0x61)
     case FConv(FCVT_LU_D(rd, rs, frm))       => Rtype(opc(0x14), frm, rd, rs, 3, 0x61)
     case FConv(  FMV_X_D(rd, rs))            => Rtype(opc(0x14), 0,   rd, rs, 0, 0x71)
     case FConv( FCVT_D_L(rd, rs, frm))       => Rtype(opc(0x14), frm, rd, rs, 2, 0x69)
     case FConv(FCVT_D_LU(rd, rs, frm))       => Rtype(opc(0x14), frm, rd, rs, 3, 0x69)
     case FConv(  FMV_D_X(rd, rs))            => Rtype(opc(0x14), 0,   rd, rs, 0, 0x79)

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

     case System( ECALL)                    =>  Itype(opc(0x1C), 0, 0, 0, 0x000)
     case System(EBREAK)                    =>  Itype(opc(0x1C), 0, 0, 0, 0x001)
     case System(  URET)                    =>  Itype(opc(0x1C), 0, 0, 0, 0x002)
     case System(  SRET)                    =>  Itype(opc(0x1C), 0, 0, 0, 0x102)
     case System(  MRET)                    =>  Itype(opc(0x1C), 0, 0, 0, 0x302)

     case System(   WFI)                    =>  Itype(opc(0x1C), 0, 0, 0, 0x105)

     case System(SFENCE_VM(rs1, rs2))       =>  Rtype(opc(0x1C), 0, 0, rs1, rs2, 0x05)

     case System( CSRRW(rd, rs1, csr))      =>  Itype(opc(0x1C), 1, rd, rs1, csr)
     case System( CSRRS(rd, rs1, csr))      =>  Itype(opc(0x1C), 2, rd, rs1, csr)
     case System( CSRRC(rd, rs1, csr))      =>  Itype(opc(0x1C), 3, rd, rs1, csr)
     case System(CSRRWI(rd, imm, csr))      =>  Itype(opc(0x1C), 5, rd, imm, csr)
     case System(CSRRSI(rd, imm, csr))      =>  Itype(opc(0x1C), 6, rd, imm, csr)
     case System(CSRRCI(rd, imm, csr))      =>  Itype(opc(0x1C), 7, rd, imm, csr)

     case UnknownInstruction                => 0

     case Internal(FETCH_MISALIGNED(_))     => 0
     case Internal(FETCH_FAULT(_))          => 0
   }

---------------------------------------------------------------------------
-- The next state function
---------------------------------------------------------------------------

string log_instruction(w::word, inst::instruction) =
    "instr " : [procID] : " " : [[c_instret(procID)]::nat] :
    " 0x" : hex64(PC) : " : " : hex32(w) : "   " : instructionToString(inst)

nat exitCode() =
    [ExitCode >> 1]::nat

-- The clock/timer factor here is arbitrary, except it that if it is
-- >= approx 200, some 32-bit -pt- tests unexpectedly pass.

nat CYCLES_PER_TIMER_TICK = 200

unit tickClock() =
{ cycles             = c_cycles(procID) + 1
; c_cycles(procID)  <- cycles
; clock             <- cycles div [CYCLES_PER_TIMER_TICK]
}

unit incrInstret() =
    c_instret(procID) <- c_instret(procID) + 1

unit checkTimers() =
{ ()
}

unit Next =
{ var interrupt = false
; initDelta ()

-- Interrupts are prioritized above synchronous traps, so first check
-- if we have a pending interrupt before fetching.

; match curInterrupt()
  { case Some(i, delegateePriv) =>
    { interrupt <- true
    ; excHandler(true, interruptIndex(i), curPrivilege, delegateePriv, PC, None)
    }
    case None =>
      match Fetch()
      { case F_Result(w) =>
        { inst = Decode(w)
        ; mark_log(LOG_INSN, log_instruction(w, inst))
        ; Run(inst)
        }
        case F_Error(inst) =>
        { mark_log(LOG_INSN, log_instruction([0::word], inst))
        ; Run(inst)
        }
      }
  }

; tickClock()

  -- fetch only if we are not handling an interrupt
; if interrupt then ()
  else match NextFetch
  { case Some(Trap(e)) =>
             { NextFetch <- None
             ; delegate = excHandlerDelegate(e.trap)
             ; excHandler(false, [e.trap]::exc_code, curPrivilege, delegate, PC, e.badaddr)
             }
    case Some(Uret) =>
             { NextFetch    <- None
             ; mark_log(LOG_INSN, ["ret-ing from " : privName(curPrivilege)
                                   : " to " : privName(User)])
             ; curPrivilege <- User
             ; MCSR.mstatus <- uret(MCSR.mstatus)
             ; PC           <- UCSR.uepc

             ; recordMStatus(MCSR.mstatus)
             }
    case Some(Sret) =>
             { NextFetch    <- None
             ; mark_log(LOG_INSN, ["ret-ing from " : privName(curPrivilege)
                                   : " to " : privName(if MCSR.mstatus.M_SPP then Supervisor else User)])
             ; curPrivilege <- if MCSR.mstatus.M_SPP then Supervisor else User
             ; MCSR.mstatus <- sret(MCSR.mstatus)
             ; PC           <- SCSR.sepc

             ; recordMStatus(MCSR.mstatus)
             }
    case Some(Mret) =>
             { NextFetch    <- None
             ; mark_log(LOG_INSN, ["ret-ing from " : privName(curPrivilege)
                                   : " to " : privName(privilege(MCSR.mstatus.M_MPP))])
             ; curPrivilege <- privilege(MCSR.mstatus.M_MPP)
             ; MCSR.mstatus <- mret(MCSR.mstatus)
             ; PC           <- MCSR.mepc

             ; recordMStatus(MCSR.mstatus)
             }
    case Some(BranchTo(pc)) =>
             { incrInstret()
             ; NextFetch    <- None
             ; PC           <- pc
             -- mstatus should not have changed, preserve previous value in the delta
             }
    case None =>
             { incrInstret()
             ; PC           <- PC + 4
             -- mstatus could have changed due to a csr write
             ; recordMStatus(MCSR.mstatus)
             }
  }
; recordPC(PC, curPrivilege)
}

-- TODO: This needs to be parameterized by an isa string, or
-- initialized from outside the model.
unit initIdent(arch::Architecture) =
{ MCSR.misa.MXL     <- archBase(arch)
; MCSR.misa.X       <- false
; MCSR.misa.U       <- true
; MCSR.misa.S       <- true
; MCSR.misa.N       <- false
; MCSR.misa.M       <- true
; MCSR.misa.I       <- true
; MCSR.misa.H       <- false
; MCSR.misa.G       <- false
; MCSR.misa.F       <- true
; MCSR.misa.D       <- true
; MCSR.misa.C       <- false
; MCSR.misa.A       <- true

; MCSR.mvendorid    <- MVENDORID
; MCSR.marchid      <- MARCHID
; MCSR.mimpid       <- MIMPID
}

unit initMachine(hartid::id) =
{ -- Startup in Mbare machine mode, with interrupts disabled.
  curPrivilege      <- Machine
; var ms = mstatus(0)

  -- initialize extension context state
; ms.M_FS           <- ext_status(Off) -- FIXME: Should be Initial?
; ms.M_XS           <- ext_status(Off)
; ms.M_SD           <- false
  -- initialize *-xlen.
; ms.M_SXL          <- MCSR.misa.MXL
; ms.M_UXL          <- MCSR.misa.MXL

; MCSR.mstatus      <- ms
; recordMStatus(ms)

  -- Setup hartid
; MCSR.mhartid      <- ZeroExtend(hartid)
  -- Initialize mtvec to lower address (other option is 0xF...FFE00)
; MCSR.&mtvec       <- ZeroExtend(0x100`16)

  -- TODO: other CSRs
}
-- This initializes each core (via setting procID appropriately) on
-- startup before execution begins.
unit initRegs(pc::nat) =
{ -- Initializing to an arbitrary value causes issues with
  -- the verifier, which assumes 0-valued initialization.
  for i in 0 .. 31 do
    gpr([i])   <- 0x0
; for i in 0 .. 31 do
    fpr([i])   <- 0x0

; NextFetch <- None
; PC        <- [pc]
; done      <- false
}
