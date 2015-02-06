---------------------------------------------------------------------------
-- CHERI MIPS TLB address translation
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

pAddr * CCA * bool * bool AddressTranslation (vAddr::vAddr, IorD::IorD, AccessType::AccessType) =
{
    unmapped, valid = CheckSegment (vAddr);
    if valid then
        match unmapped
        {
            case Some (pAddr, cca) => pAddr, cca, false, false
                case None =>
                match LookupTLB (vAddr<63:62>, vAddr<39:13>)
                {
                    case Nil =>
                    {
                        exc = if  AccessType == LOAD then XTLBRefillL
                              else XTLBRefillS;
                        _ = SignalTLBException (exc, CP0.EntryHi.ASID, vAddr);
                        UNKNOWN
                    }
                    case list {(_, e)} =>
                    {
                        EvenOddBit = match e.Mask
                        {
                            case 0b0000_0000_0000 => 12
                                case 0b0000_0000_0011 => 14
                                case 0b0000_0000_1111 => 16
                                case 0b0000_0011_1111 => 18
                                case 0b0000_1111_1111 => 20
                                case 0b0011_1111_1111 => 22
                                case 0b1111_1111_1111 => 24
                                case _                => #UNPREDICTABLE ("TLB: bad mask")
                        };

                        S, L, PFN, C, D, V =
                        if vAddr<EvenOddBit> then
                            e.S1, e.L1, e.PFN1, e.C1, e.D1, e.V1
                        else
                            e.S0, e.L0, e.PFN0, e.C0, e.D0, e.V0;

                        if V then
                            if not D and AccessType == STORE then
                                { _ = SignalTLBException (Mod, e.ASID, vAddr); UNKNOWN }
                            else
                            {
                                PFN_     = [PFN]   :: bool list;
                                vAddr_   = [vAddr] :: bool list;
                                pAddr    = PFN_<27:EvenOddBit-12>
                                   : vAddr_<EvenOddBit-1:0>;
                                ([pAddr], C, S, L)
                            }
                        else
                        {
                            exc = if AccessType == LOAD then TLBL else TLBS;
                            _ = SignalTLBException (exc, e.ASID, vAddr);
                            UNKNOWN
                        }
                    }
                    case _ => #UNPREDICTABLE ("TLB: multiple matches")
                }
        }
    else
    {
        CP0.BadVAddr <- PCC.base + vAddr;
        SignalException (if AccessType == LOAD then AdEL else AdES);
        UNKNOWN
    }
}

TLBEntry ModifyTLB (ie::TLBEntry) =
{
   eHi = CP0.EntryHi;
   eLo1 = CP0.EntryLo1;
   eLo0 = CP0.EntryLo0;
   var e = ie;
   e.Mask <- CP0.PageMask.Mask;
   e.R <- eHi.R;
   e.VPN2 <- eHi.VPN2;
   e.ASID <- eHi.ASID;
   e.S1 <- eLo1.S;
   e.L1 <- eLo1.L;
   e.PFN1 <- eLo1.PFN;
   e.C1 <- eLo1.C;
   e.D1 <- eLo1.D;
   e.V1 <- eLo1.V;
   e.G <- eLo1.G and eLo0.G;
   e.S0 <- eLo0.S;
   e.L0 <- eLo0.L;
   e.PFN0 <- eLo0.PFN;
   e.C0 <- eLo0.C;
   e.D0 <- eLo0.D;
   e.V0 <- eLo0.V;
   return e
}

define TLBR =
   if !CP0.Status.CU0 and !KernelMode then
     SignalException(CpU)
   else
   {
     i = CP0.Index.Index;
     e = if [i] >= TLBEntries then
           TLB_direct (i<6:0>)
         else
           TLB_assoc ([i]);
     CP0.PageMask.Mask <- e.Mask;
     CP0.EntryHi.R <- e.R;
     CP0.EntryHi.VPN2 <- e.VPN2;
     CP0.EntryHi.ASID <- e.ASID;
     CP0.EntryLo1.S <- e.S1;
     CP0.EntryLo1.L <- e.L1;
     CP0.EntryLo1.PFN <- e.PFN1;
     CP0.EntryLo1.C <- e.C1;
     CP0.EntryLo1.D <- e.D1;
     CP0.EntryLo1.V <- e.V1;
     CP0.EntryLo1.G <- e.G;
     CP0.EntryLo0.S <- e.S0;
     CP0.EntryLo0.L <- e.L0;
     CP0.EntryLo0.PFN <- e.PFN0;
     CP0.EntryLo0.C <- e.C0;
     CP0.EntryLo0.D <- e.D0;
     CP0.EntryLo0.V <- e.V0;
     CP0.EntryLo0.G <- e.G
   }
