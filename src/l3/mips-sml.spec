---------------------------------------------------------------------------
-- Model of the 64-bit MIPS ISA (MIPS III with some extra instructions)
-- (c) Anthony Fox, University of Cambridge
---------------------------------------------------------------------------

type mAddr = bits(37)

nat PSIZE = 40         -- 40-bit physical memory

declare done :: bool   -- Flag to request termination

--------------------------------------------------
-- Gereral purpose register access
--------------------------------------------------

component GPR (n::reg) :: dword
{
   value = if n == 0 then 0 else gpr(n)
   assign value = when n <> 0 do { gpr(n) <- value; mark_log (2, log_w_gpr (n, value)) }
}

unit dumpRegs () =
{
    mark_log (0, "======   Registers   ======")
  ; mark_log (0, "Core = " : [[procID]::nat])
  ; mark_log (0, "PC     " : hex64(PC))
  ; for i in 0 .. 31 do
      mark_log (0, "Reg " : (if i < 10 then " " else "") : [i] : " " :
                hex64(GPR([i])))
}

--------------------------------------------------
-- HI/LO registers
--------------------------------------------------

declare
{
   UNPREDICTABLE_LO :: unit -> unit
   UNPREDICTABLE_HI :: unit -> unit
}

component HI :: dword
{
   value = match hi { case Some (v) => v
                      case None => { UNPREDICTABLE_HI (); UNKNOWN }
                    }
   assign value = { hi <- Some (value); mark_log (2, log_w_hi (value)) }
}

component LO :: dword
{
   value = match lo { case Some (v) => v
                      case None => { UNPREDICTABLE_LO (); UNKNOWN }
                    }
   assign value = { lo <- Some (value); mark_log (2, log_w_lo (value)) }
}


--------------------------------------------------
-- Main memory
--------------------------------------------------

word flip_endian_word (w::word) =
   match w { case 'a`8 b`8 c`8 d' => d : c : b : a }

dword flip_endian_dword (dw::dword) =
  match dw { case 'a`8 b`8 c`8 d`8 e`8 f`8 g`8 h' =>
                  h : g : f : e : d : c : b : a }

--------------------------------------------------
-- CP0 register access
--------------------------------------------------

component CPR (n::nat, reg::bits(5), sel::bits(3)) :: dword
{
   value =
      match n, reg, sel
      {
         case 0,  0, 0 => [CP0.&Index]
         case 0,  1, 0 => [CP0.&Random]
         case 0,  2, 0 =>  CP0.&EntryLo0
         case 0,  3, 0 =>  CP0.&EntryLo1
         case 0,  4, 0 =>  CP0.&Context
         case 0,  4, 2 =>  CP0.UsrLocal
         case 0,  5, 0 => [CP0.&PageMask]
         case 0,  6, 0 => [CP0.&Wired]
         case 0,  7, 0 => [CP0.&HWREna]
         case 0,  8, 0 =>  CP0.BadVAddr
         case 0,  8, 1 => [CP0.EInstr]
         case 0,  9, 0 => [CP0.Count]
         case 0, 10, 0 =>  CP0.&EntryHi
         case 0, 11, 0 => [CP0.Compare]
         case 0, 12, 0 => [CP0.&Status]
         case 0, 13, 0 => [CP0.&Cause]
         case 0, 14, 0 =>  CP0.EPC
         case 0, 15, 0 => [CP0.PRId]
         case 0, 15, 1 => ZeroExtend( ([totalCore - 1] :: bits(16))
                                    : ([procID] :: bits(16)) )
         case 0, 15, 6 => ZeroExtend( ([totalCore - 1] :: bits(16))
                                    : ([procID] :: bits(16)) )
         case 0, 16, 0 => [CP0.&Config]
         case 0, 16, 1 => [CP0.&Config1]
         case 0, 16, 2 => [CP0.&Config2]
         case 0, 16, 3 => [CP0.&Config3]
         case 0, 16, 4 => 1 -- Mimic BERI
         case 0, 16, 5 => 1 -- Mimic BERI
         case 0, 16, 6 => [CP0.&Config6]
         case 0, 17, 0 => [CP0.LLAddr]
         case 0, 20, 0 =>  CP0.&XContext
         case 0, 23, 0 => [CP0.Debug]
         case 0, 26, 0 => [CP0.ErrCtl]
         case 0, 30, 0 =>  CP0.ErrorEPC
         case _ => UNKNOWN
      }
   assign value =
   {
      mark_log (2, log_w_c0 (reg, value));
      match n, reg, sel
      {
         case 0,  0, 0 => CP0.Index.Index <- value<7:0>
         case 0,  2, 0 => CP0.&EntryLo0 <- value
         case 0,  3, 0 => CP0.&EntryLo1 <- value
         case 0,  4, 0 => CP0.Context.PTEBase <- value<63:23>
         case 0,  4, 2 => CP0.UsrLocal <- value
         case 0,  5, 0 => CP0.PageMask.Mask <- value<24:13>
         case 0,  6, 0 => {
                            CP0.Wired.Wired <- value<7:0>;
                            CP0.Random.Random <- [TLBEntries-1]
                          }
         case 0,  7, 0 => {
                            CP0.HWREna.CPUNum <- value<0>;
                            CP0.HWREna.CC     <- value<2>;
                            CP0.HWREna.CCRes  <- value<3>;
                            CP0.HWREna.UL     <- value<29>
                          }
         case 0,  9, 0 => CP0.Count <- value<31:0>
         case 0, 10, 0 => CP0.&EntryHi <- value
         case 0, 11, 0 => {
                            CP0.Compare <- value<31:0>;
                            CP0.Cause.IP<7> <- false;
                            CP0.Cause.TI <- false
                          }
         case 0, 12, 0 => CP0.&Status <- value<31:0>
         case 0, 13, 0 => CP0.&Cause <- value<31:0>
         case 0, 14, 0 => CP0.EPC <- value
         case 0, 16, 0 => CP0.Config.K0 <- value<2:0>
         case 0, 16, 2 => CP0.Config2.SU <- value<15:12>
         case 0, 16, 6 => CP0.Config6.LTLB <- value<2>
         case 0, 20, 0 => CP0.XContext.PTEBase <- value<63:33>
         case 0, 23, 0 => {CP0.Debug <- value<31:0>; done <- true}
         case 0, 26, 0 => {CP0.ErrCtl <- value<31:0>; dumpRegs()}
         case 0, 30, 0 => CP0.ErrorEPC <- value
         case _ => unmark_log(2)
      }
   }
}

-------------------------
-- CACHE op, offset(base)
-------------------------
define CACHE (base::reg, opn::bits(5), offset::bits(16)) =
   if !CP0.Status.CU0 and !KernelMode then
      SignalException(CpU)
   else
   {
      vAddr = GPR(base) + SignExtend(offset);
      pAddr, cca = AddressTranslation (vAddr, DATA, LOAD);
      nothing
   }

---------------
-- RDHWR rt, rd
---------------

define RDHWR (rt::reg, rd::reg) =
   if CP0.Status.CU0 or KernelMode or CP0.&HWREna<[rd]::nat> then
      match rd
      {
         case  0 => GPR(rt) <- [procID]
         case  2 => GPR(rt) <- SignExtend(CP0.Count)
         case  3 => GPR(rt) <- 1
         case 29 => GPR(rt) <- CP0.UsrLocal
         case 30 => GPR(rt) <- [totalCore - 1]
         case _  => SignalException(ResI)
      }
   else
      SignalException(ResI)
