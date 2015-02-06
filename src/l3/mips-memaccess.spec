---------------------------------------------------------------------------
-- Model of the 64-bit MIPS ISA (MIPS III with some extra instructions)
-- (c) Anthony Fox, University of Cambridge
---------------------------------------------------------------------------

-- Pimitive memory load (with memory-mapped devices)

dword LoadMemory (MemType::bits(3), AccessLength::bits(3), vAddr::vAddr,
                          IorD::IorD, AccessType::AccessType, link::bool) =
{
    var pAddr;
    tmp, CCA = AddressTranslation (vAddr, IorD, AccessType);
    pAddr <- tmp;
    pAddr<2:0> <- match MemType
    {
        case 0 => (pAddr<2:0> ?? ReverseEndian^3)
        case 1 => (pAddr<2:0> ?? (ReverseEndian^2 : '0'))
        case 3 => (pAddr<2:0> ?? (ReverseEndian : '00'))
        case 7 =>  pAddr<2:0>
        case _ => #UNPREDICTABLE ("bad access length")
    };
    pAddr <- if BigEndianMem then pAddr else pAddr && ~0b111;
    if not exceptionSignalled then
    {
        a = pAddr<39:3>;
        var ret;

        var found = false;
        if a == JTAG_UART.base_address then
        {
            found <- true;
            ret <- flip_endian_word (JTAG_UART.&data) :
                   flip_endian_word (JTAG_UART.&control);
            when pAddr<2:0> == 0 do JTAG_UART_load
        }
        else for core in 0 .. totalCore - 1 do
            when a >=+ PIC_base_address([core]) and
                  a <+ PIC_base_address([core]) + 1072 do
            {
                found <- true;
                ret <- PIC_load([core], a)
            };

        if link then
        {
            LLbit <- Some (true);
            CP0.LLAddr <- [pAddr]
        }
        else
            LLbit <- None;

        when found == false do
            ret <- ReadData (a);

        return ret
    }
    else return UNKNOWN
}

-- Pimitive memory store. Big-endian.

bool StoreMemory (MemType::bits(3), AccessLength::bits(3), MemElem::dword,
                   vAddr::vAddr, IorD::IorD, AccessType::AccessType, cond::bool) =
{
    var pAddr;
    var sc_success = false;
    tmp, CCA = AddressTranslation (vAddr, IorD, AccessType);
    pAddr <- tmp;
    pAddr<2:0> <- match MemType
    {
        case 0 => (pAddr<2:0> ?? ReverseEndian^3)
        case 1 => (pAddr<2:0> ?? (ReverseEndian^2 : '0'))
        case 3 => (pAddr<2:0> ?? (ReverseEndian : '00'))
        case 7 =>  pAddr<2:0>
        case _ => #UNPREDICTABLE ("bad access length")
    };
    pAddr <- if BigEndianMem then pAddr else pAddr && ~0b111;
    when not exceptionSignalled do
    {
        a = pAddr<39:3>;
        l = 64 - ([AccessLength] + 1 + [vAddr<2:0>]) * 0n8;
        mask`64 = [2 ** (l + ([AccessLength] + 1) * 0n8) - 2 ** l];

        var found = false;
        if a == JTAG_UART.base_address then
        {
            found <- true;
            JTAG_UART_store (mask, MemElem)
        }
        else for core in 0 .. totalCore - 1 do
            when a >=+ PIC_base_address([core]) and
                 a <+ PIC_base_address([core]) + 1072 do
            {
                found <- true;
                PIC_store([core], a, mask, MemElem)
            };

        when cond do match LLbit
        {
            case None => #UNPREDICTABLE("conditional store: LLbit not set")
            case Some (false) => sc_success <- false
            case Some (true) =>
                if CP0.LLAddr<39:5> == pAddr<39:5> then
                    sc_success <- true
                else #UNPREDICTABLE("conditional store: address does not match previous LL address")
        };

        LLbit <- None;

        when not found do
        {
            for core in 0 .. totalCore - 1 do
                when core <> [procID] and
                     c_LLbit([core]) == Some (true) and
                     c_CP0([core]).LLAddr<39:5> == pAddr<39:5> do
                        c_LLbit([core]) <- Some (false);
            when not cond or sc_success do WriteData(a, MemElem, mask)
        }
    };
    sc_success
}

--------------------------------------------------
-- Instruction fetch
--------------------------------------------------

word option Fetch =
{
   CP0.Random.Random <- if CP0.Random.Random == CP0.Wired.Wired then
                           [TLBEntries - 1]
                        else
                           CP0.Random.Random - 1;

   when CP0.Compare == CP0.Count do
   {
      CP0.Cause.IP<7> <- true;
      CP0.Cause.TI <- true
   };

   when CP0.Status.IE and not (CP0.Status.EXL or CP0.Status.ERL) do
   {
      -- If any interrupts pending, raise an exception
      when (CP0.Status.IM<7:2> && CP0.Cause.IP<7:2>) <> 0 do
        SignalException (Int)
   };

   if exceptionSignalled then
      None
   else if PC<1:0> == 0 then
   {
      pc, cca = AddressTranslation (PC, INSTRUCTION, LOAD);
      if exceptionSignalled then None else Some (ReadInst (pc))
   }
   else
   {
      CP0.BadVAddr <- PC;
      SignalException (AdEL);
      None
   }
}

-----------------------------------
-- JALR rs (rd = 31 implied)
-- JALR rd, rs
-----------------------------------
define Branch > JALR (rs::reg, rd::reg) =
{
   temp = GPR(rs);
   GPR(rd) <- PC + 8;
   BranchTo <- Some (temp)
}
