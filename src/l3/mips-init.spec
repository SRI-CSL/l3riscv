---------------------------------------------------------------------------
-- Model of the 64-bit MIPS ISA (MIPS III with some extra instructions)
-- (c) Anthony Fox, University of Cambridge
---------------------------------------------------------------------------

TLBEntry initTLB = { var e::TLBEntry; e.R <- '10'; return e }

unit initMips (pc::nat, uart::nat) =
{
   -- Configuration register (mimic BERI)
   CP0.Config.M   <- true;      -- true if config register 1 exists
   CP0.Config.BE  <- true;      -- big-endian
   CP0.Config.MT  <- 1;         -- standard TLB
   CP0.Config.AR  <- 0;         -- 0 = revision 1, 1 = revision 2
   CP0.Config.AT  <- 2;         -- MIPS64 with access to all address segments

   -- Configuration register 1 (mimic BERI)
   CP0.Config1.M  <- true;      -- true if config register 2 exists
   CP0.Config1.MMUSize <- 15;   -- TLB has MMUSize+1 entries
   CP0.Config1.IS <- 3;         -- Icache sets per way
   CP0.Config1.IL <- 4;         -- Icache line size
   CP0.Config1.IA <- 0;         -- Icache associativity
   CP0.Config1.DS <- 3;         -- Dcache sets per way
   CP0.Config1.DL <- 4;         -- Dcache line size
   CP0.Config1.DA <- 0;         -- Dcache associativity
   CP0.Config1.C2 <- hasCP2;    -- Coprocessor 2 available?
   CP0.Config1.MD <- false;     -- MDMX ASE implemented?
   CP0.Config1.PCR <- false;    -- Performance counter registers implemented?
   CP0.Config1.WR <- false;     -- Watch registers implemented? (true on BERI)
   CP0.Config1.CA <- false;     -- Code compression (MIPS16) implemented?
   CP0.Config1.EP <- false;     -- EJTAG implemented?
   CP0.Config1.FP <- false;     -- FPU implemented?

   -- Configuration register 2 (mimic BERI)
   CP0.Config2.M  <- true;      -- true if config register 3 exists
   CP0.Config2.TU <- 0;         -- Tertiary cache control
   CP0.Config2.TS <- 0;         -- Tertiary cache sets per way
   CP0.Config2.TL <- 0;         -- Tertiary cache line size
   CP0.Config2.TA <- 0;         -- Tertiary cache associativity
   CP0.Config2.SU <- 3;         -- Secondary cache control
   CP0.Config2.SS <- 8;         -- Secondary cache sets per way
   CP0.Config2.SL <- 4;         -- Secondary cache line size
   CP0.Config2.SA <- 0;         -- Secondary cache associativity

   -- Configuration register 3 (mimic BERI)
   CP0.Config3.M  <- true;      -- true if config register 4 exists
   CP0.Config3.ULRI <- true;    -- UserLocal register implemented?
   CP0.Config3.DSPP <- false;   -- MIPS DSPASE implemented?
   CP0.Config3.LPA  <- false;   -- Large physical addr support and
                                -- page grain register present?
   CP0.Config3.VEIC <- false;   -- External interrupt controller present?
   CP0.Config3.VInt <- false;   -- Vectored interrupts implemented?
   CP0.Config3.SP   <- false;   -- Small (1kB) page support?
   CP0.Config3.MT   <- false;   -- MIPS MTASE implemented?
   CP0.Config3.SM   <- false;   -- SmartMIPS ASI implemented?
   CP0.Config3.TL   <- false;   -- Trace Logic implemented?

   -- Configuration register 6 (mimic BERI)
   CP0.Config6.TLBSize <- 143;
   CP0.Config6.LTLB <- false;   -- Enable large TLB?

   CP0.&Status <- 0x0;          -- reset to kernel mode (interrupts disabled)
   CP0.Status.BEV <- true;
   CP0.Status.KSU <- '00';
   CP0.Status.EXL <- false;
   CP0.Status.ERL <- false;
   CP0.Status.KX <- true;
   CP0.Status.SX <- true;
   CP0.Status.UX <- true;
   CP0.Count <- 0;
   CP0.Compare <- 0;
   CP0.PRId <- 0x400;           -- processor ID
   CP0.Index.P <- false;
   CP0.Index.Index <- 0x0;
   CP0.Random.Random <- [TLBEntries-1];
   CP0.Wired.Wired <- 0;
   CP0.&HWREna <- 0;
   for i in 0 .. 127 do TLB_assoc([i]) <- initTLB;
   BranchDelay <- None;
   BranchTo <- None;
   LLbit <- None;
   hi <- None;
   lo <- None;
   PC <- [pc];
   COP2Init();
   done <- false;
   for i in 0 .. 31 do gpr([i]) <- 0xAAAAAAAAAAAAAAAA;
   PIC_initialise (0x7f804000 + [procID] * 0x4000);
   when procID == 0 do
   {
      JTAG_UART_initialise (uart);
      InitMEM
   }
}
