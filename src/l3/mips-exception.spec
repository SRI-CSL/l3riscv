---------------------------------------------------------------------------
-- Standard MIPS exception mechanism
-- (c) Anthony Fox, University of Cambridge
---------------------------------------------------------------------------

--------------------------------------------------
-- Exceptions
--------------------------------------------------

construct ExceptionType
{
   Int, Mod, TLBL, TLBS, AdEL, AdES, Sys, Bp, ResI, CpU, Ov, Tr, C2E,
   XTLBRefillL, XTLBRefillS
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
   vectorOffset = if (ExceptionType == XTLBRefillL or
                      ExceptionType == XTLBRefillS)
                      and not CP0.Status.EXL then
                     0x080`30
                  else
                     0x180;
   when IsSome(currentInst) do CP0.EInstr <- ValOf(currentInst);
   CP0.Cause.ExcCode <- ExceptionCode (ExceptionType);
   CP0.Status.EXL <- true;
   vectorBase = if CP0.Status.BEV then
                   0xFFFF_FFFF_BFC0_0200`64
                else
                   0xFFFF_FFFF_8000_0000;
   BranchDelay <- None;
   BranchTo <- None;
   PC <- vectorBase<63:30> : (vectorBase<29:0> + vectorOffset);
   exceptionSignalled <- true;
   mark_log(2, log_sig_exception(ExceptionCode(ExceptionType)))
}

unit SignalCP2UnusableException = {CP0.Cause.CE <- '10'; SignalException(CpU)}

-----------------------------------
-- ERET instruction
-----------------------------------
define ERET =
{
   CheckBranch;
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
      LLbit <- Some (false)
   }
   else
     SignalException (CpU)
}
