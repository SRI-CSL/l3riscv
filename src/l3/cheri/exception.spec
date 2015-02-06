---------------------------------------------------------------------------
-- CHERI exception mechanism
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

--------------------------------------------------
-- Exceptions
--------------------------------------------------

construct ExceptionType
{
    Int, Mod, TLBL, TLBS, AdEL, AdES, Sys, Bp, ResI, CpU, Ov, Tr,
    CTLBL, CTLBS, C2E, XTLBRefillL, XTLBRefillS
}

construct CapException
{
    capExcNone,              -- None
    capExcLength,            -- Length Violation
    capExcTag,               -- Tag Violation
    capExcSeal,              -- Seal Violation
    capExcType,              -- Type Violation
    capExcCall,              -- Call Trap
    capExcRet,               -- Return Trap
    capExcUnderflowTSS,      -- Underflow of trusted system stack
    capExcUser,              -- User-defined Permision Violation
    capExcTLBNoStore,        -- User-defined Permision Violation
    capExcGlobal,            -- Global Violation
    capExcPermExe,           -- Permit_Execute Violation
    capExcPermLoad,          -- Permit_Load Violation
    capExcPermStore,         -- Permit_Store Violation
    capExcPermLoadCap,       -- Permit_Load_Capability Violation
    capExcPermStoreCap,      -- Permit_Store_Capability Violation
    capExcPermStoreLocalCap, -- Permit_Store_Local_Capability Violation
    capExcPermSeal,          -- Permit_Seal Violation
    capExcPermSetType,       -- Permit_Set_Type Violation
    capExcAccEPCC,           -- Access_EPCC
    capExcAccKDC,            -- Access_KDC
    capExcAccKCC,            -- Access_KCC
    capExcAccKR1C,           -- Access_KR1C
    capExcAccKR2C            -- Access_KR2C
}

bits(5) ExceptionCode (ExceptionType::ExceptionType) =
    match ExceptionType
    {
        case Int         => 0x00 -- Interrupt
        case Mod         => 0x01 -- TLB modification exception
        case TLBL        => 0x02 -- TLB exception (load or fetch)
        case TLBS        => 0x03 -- TLB exception (store)
        case AdEL        => 0x04 -- Address error (load or fetch)
        case AdES        => 0x05 -- Address error (store)
        case Sys         => 0x08 -- Syscall
        case Bp          => 0x09 -- Breakpoint
        case ResI        => 0x0a -- Reserved instruction
        case CpU         => 0x0b -- Coprocessor Unusable
        case Ov          => 0x0c -- Arithmetic overflow
        case Tr          => 0x0d -- Trap
        case CTLBL       => 0x10 -- Capability TLB Load exception
        case CTLBS       => 0x11 -- Capability TLB Store exception
        case C2E         => 0x12 -- C2E coprocessor 2 exception
        case XTLBRefillL => 0x02
        case XTLBRefillS => 0x03
    }

unit SignalException (ExceptionType::ExceptionType) =
{
    when not CP0.Status.EXL do
    {
        if IsSome (BranchDelay) then
        {
            CP0.EPC <- PC - 4;
            CP0.Cause.BD <- true
        }
        else
        {
            CP0.EPC <- PC;
            CP0.Cause.BD <- false
        }
    };
    vectorOffset =
        if (ExceptionType == XTLBRefillL or ExceptionType == XTLBRefillS)
            and not CP0.Status.EXL then
            0x080`30
        else if (ExceptionType == C2E and
            (capcause.ExcCode == 0x5 {-capExcCall-} or capcause.ExcCode == 0x6 {-capExcRet-})) then
            0x280
        else
            0x180;
    when IsSome(currentInst) do CP0.EInstr <- ValOf(currentInst);
    CP0.Cause.ExcCode <- ExceptionCode (ExceptionType);
    CP0.Status.EXL <- true;
    vectorBase =
        if CP0.Status.BEV then
            0xFFFF_FFFF_BFC0_0200`64
        else
            0xFFFF_FFFF_8000_0000;
    BranchDelay <- None;
    BranchTo <- None;
    exceptionSignalled <- true;

    -- move PCC to EPCC
    var new_epcc = PCC;
    new_epcc.offset <- PC;
    EPCC <- new_epcc;
    -- move KCC to PCC
    PCC <- KCC;

    PC <- vectorBase<63:30> : (vectorBase<29:0> + vectorOffset);
    mark_log (2, log_sig_exception(ExceptionCode(ExceptionType)))
}

unit SignalCP2UnusableException = {CP0.Cause.CE <- '10'; SignalException(CpU)}

unit SignalCapException_internal (capException::CapException, regNum::bits(8)) =
{
    capcause.ExcCode <- match capException
    {
        case capExcNone              => 0x0
        case capExcLength            => 0x1
        case capExcTag               => 0x2
        case capExcSeal              => 0x3
        case capExcType              => 0x4
        case capExcCall              => 0x5
        case capExcRet               => 0x6
        case capExcUnderflowTSS      => 0x7
        case capExcUser              => 0x8
        case capExcTLBNoStore        => 0x9
        case capExcGlobal            => 0x10
        case capExcPermExe           => 0x11
        case capExcPermLoad          => 0x12
        case capExcPermStore         => 0x13
        case capExcPermLoadCap       => 0x14
        case capExcPermStoreCap      => 0x15
        case capExcPermStoreLocalCap => 0x16
        case capExcPermSeal          => 0x17
        case capExcPermSetType       => 0x18
        case capExcAccEPCC           => 0x1a
        case capExcAccKDC            => 0x1b
        case capExcAccKCC            => 0x1c
        case capExcAccKR1C           => 0x1d
        case capExcAccKR2C           => 0x1e
    };
    capcause.RegNum  <- regNum;
    mark_log (2, "Cap exception - cause: 0x" : [capcause.ExcCode] : ", reg: 0x" : [capcause.RegNum]);
    SignalException(C2E)
}

unit SignalCapException (capException::CapException, regNum::bits(5)) =
    SignalCapException_internal (capException, ZeroExtend(regNum))

unit SignalCapException_noReg (capException::CapException) =
    SignalCapException_internal (capException, 0xff)

unit SignalCapException_v (regNum::bits(5)) =
   match regNum
   {
     case 31 => SignalCapException(capExcAccEPCC, ZeroExtend(regNum))
     case 30 => SignalCapException(capExcAccKDC, ZeroExtend(regNum))
     case 29 => SignalCapException(capExcAccKCC, ZeroExtend(regNum))
     case 27 => SignalCapException(capExcAccKR1C, ZeroExtend(regNum))
     case 28 => SignalCapException(capExcAccKR2C, ZeroExtend(regNum))
     case _ => ()
   }

-----------------------------------
-- ERET instruction
-----------------------------------
define ERET =
   if CP0.Status.CU0 or KernelMode then
   {
      if CP0.Status.ERL then
      {
          PC <- CP0.ErrorEPC - 4;
          CP0.Status.ERL <- false
      }
      else
      {
          PC <- CP0.EPC - 4;
          CP0.Status.EXL <- false
      };
      LLbit <- Some (false);
      -- move EPCC to PCC
      PCC <- EPCC
   }
   else
      SignalException (CpU)
