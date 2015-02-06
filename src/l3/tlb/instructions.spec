---------------------------------------------------------------------------
-- Instructions of the MIPS TLB
-- (c) Anthony Fox, University of Cambridge
---------------------------------------------------------------------------

--------------------------------------------------
-- TLB instructions
--------------------------------------------------

define TLBP =
   if !CP0.Status.CU0 and !KernelMode then
     SignalException(CpU)
   else
     match LookupTLB (CP0.EntryHi.R, CP0.EntryHi.VPN2)
     {
        case Nil =>
           {
              CP0.Index.P <- true;
              CP0.Index.Index <- UNKNOWN
           }
        case list {(i, e)} =>
           {
              CP0.Index.P <- false;
              CP0.Index.Index <- i
           }
        case _ => #UNPREDICTABLE ("TLB: multiple matches")
     }

define TLBWI =
   if !CP0.Status.CU0 and !KernelMode then
      SignalException(CpU)
   else if [CP0.Index.Index] >= TLBEntries then
   {
      j = CP0.EntryHi.VPN2<6:0>;
      TLB_direct (j) <- ModifyTLB (TLB_direct (j))
   }
   else
   {
      i`4 = [CP0.Index.Index];
      TLB_assoc (i) <- ModifyTLB (TLB_assoc (i))
   }

define TLBWR =
   if !CP0.Status.CU0 and !KernelMode then
      SignalException(CpU)
   else if CP0.Config6.LTLB then
   {
      j = CP0.EntryHi.VPN2<6:0>;
      old = TLB_direct (j);
      TLB_direct (j) <- ModifyTLB (old);
      when old.V0 and old.V1 do TLB_assoc ([CP0.Random.Random]) <- old
   }
   else
   {
      j = CP0.Random.Random;
      TLB_assoc ([j]) <- ModifyTLB (TLB_assoc ([j]))
   }
