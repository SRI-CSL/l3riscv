---------------------------------------------------------------------------
-- CHERI extra fields in the EntryLo type
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

register EntryLo :: bits(64)
{
    63 : S        -- Disable store capability instruction
    62 : L        -- Disable load capability instruction
  33-6 : PFN      -- Page Frame Number
   5-3 : C        -- Cacheability and Coherency Attribute
     2 : D        -- Dirty bit
     1 : V        -- Valid bit
     0 : G        -- Global bit
}

record TLBEntry
{
   Mask :: bits(12)
   R    :: bits(2)
   VPN2 :: bits(27)
   G    :: bool
   ASID :: bits(8)
   S0   :: bool
   S1   :: bool
   L0   :: bool
   L1   :: bool
   PFN0 :: bits(28)
   PFN1 :: bits(28)
   C0   :: bits(3)
   C1   :: bits(3)
   D0   :: bool
   D1   :: bool
   V0   :: bool
   V1   :: bool
}

construct IorD { INSTRUCTION, DATA }
construct AccessType { LOAD, STORE, CLOAD, CSTORE }
