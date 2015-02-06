---------------------------------------------------------------------------
-- MIPS default memory
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

declare MEM :: mAddr -> dword -- physical memory (37 bits), doubleword access

unit InitMEM = MEM <- InitMap (0x0) 

dword ReadData (pAddr::mAddr) =
{
    data = MEM(pAddr);
    mark_log (2, log_r_mem (pAddr, data));
    data
}

unit WriteData (pAddr::mAddr, data::dword, mask::dword) =
{
    MEM(pAddr) <- MEM(pAddr) && ~mask || data && mask;
    mark_log (2, log_w_mem (pAddr, mask, data))
}

word ReadInst (a::pAddr) = if a<2> then MEM (a<39:3>)<31:0> else MEM (a<39:3>)<63:32>

-- sml helper function
unit WriteDWORD (pAddr::mAddr, data::dword) =
    MEM(pAddr) <- data

-- sml helper function
unit Write256 (pAddr::bits(35), data::bits(256)) =
{
    MEM(pAddr:'00') <- data<63:0>;
    MEM(pAddr:'01') <- data<127:64>;
    MEM(pAddr:'10') <- data<191:128>;
    MEM(pAddr:'11') <- data<255:192>
}
