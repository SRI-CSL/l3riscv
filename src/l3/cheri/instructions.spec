---------------------------------------------------------------------------
-- Implementation of the CHERI instructions
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

bool register_inaccessible(cb::reg) =
{
    perms = Perms(PCC.perms);
    return (cb == 31 and not perms.Access_EPCC
         or cb == 30 and not perms.Access_KDC
         or cb == 29 and not perms.Access_KCC
         or cb == 27 and not perms.Access_KR1C
         or cb == 28 and not perms.Access_KR2C)
}

-- only works for non empty lists
bool list SignExtendBitString(w::nat, x::bool list) = PadLeft (Head(x), w, x)

-----------------------------------
-- dump capability registers
-----------------------------------
define COP2 > CHERICOP2 > DumpCapReg = dumpCRegs()

-----------------------------------
-- CGetBase rd, cb
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetBase (rd::reg, cb::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else
        GPR(rd) <- CAPR(cb).base

-----------------------------------
-- CGetOffset rd, cb
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetOffset (rd::reg, cb::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else
        GPR(rd) <- CAPR(cb).offset

-----------------------------------
-- CGetLen rd, cb
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetLen (rd::reg, cb::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else
        GPR(rd) <- CAPR(cb).length

-----------------------------------
-- CGetTag rd, cb
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetTag (rd::reg, cb::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else
    {
        GPR(rd)<0> <- CAPR(cb).tag;
        GPR(rd)<63:1> <- 0
    }

-----------------------------------
-- CGetSealed rd, cb
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetSealed (rd::reg, cb::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else
    {
        GPR(rd)<0> <- CAPR(cb).sealed;
        GPR(rd)<63:1> <- 0
    }

-----------------------------------
-- CGetPerm rd, cb
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetPerm (rd::reg, cb::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else
    {
        GPR(rd)<30:0> <- CAPR(cb).perms;
        GPR(rd)<63:31> <- 0
    }

-----------------------------------
-- CGetType rd, cb
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetType (rd::reg, cb::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else
    {
        GPR(rd)<23:0> <- CAPR(cb).otype;
        GPR(rd)<63:24> <- 0
    }

-----------------------------------
-- CGetPCC cd
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetPCC (cd::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException_v(cd)
    else
    {
        var new_cap = PCC;
        new_cap.offset <- PC;
        CAPR(cd) <- new_cap
    }

-----------------------------------
-- CGetCause rd
-----------------------------------
define COP2 > CHERICOP2 > CGet > CGetCause (rd::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if not Perms(PCC.perms).Access_EPCC then
        SignalCapException_noReg(capExcAccEPCC)
    else
    {
        GPR(rd)<7:0> <- capcause.RegNum;
        GPR(rd)<15:8> <- capcause.ExcCode;
        GPR(rd)<63:16> <- 0
    }

-----------------------------------
-- CSetCause rt
-----------------------------------
define COP2 > CHERICOP2 > CSet > CSetCause (rt::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if not Perms(PCC.perms).Access_EPCC then
        SignalCapException_noReg(capExcAccEPCC)
    else
    {
        capcause.ExcCode <- GPR(rt)<15:8>;
        capcause.RegNum <- GPR(rt)<7:0>
    }

-----------------------------------
-- CIncBase
-----------------------------------
define COP2 > CHERICOP2 > CSet > CIncBase (cd::reg, cb::reg, rt::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException_v(cd)
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if not CAPR(cb).tag and GPR(rt) <> 0 then
        SignalCapException(capExcTag,cb)
    else if CAPR(cb).sealed and GPR(rt) <> 0 then
        SignalCapException(capExcSeal,cb)
    else if GPR(rt) >+ CAPR(cb).length then
        SignalCapException(capExcLength,cb)
    else
    {
        var new_cap     = CAPR(cb);
        new_cap.base   <- CAPR(cb).base + GPR(rt);
        new_cap.length <- CAPR(cb).length - GPR(rt);
        CAPR(cd) <- new_cap
    }

-----------------------------------
-- CIncOffset
-----------------------------------
define COP2 > CHERICOP2 > CSet > CIncOffset (cd::reg, cb::reg, rt::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException_v(cd)
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if CAPR(cb).tag and CAPR(cb).sealed then
        SignalCapException(capExcSeal,cb)
    else
    {
        var new_cap     = CAPR(cb);
        new_cap.offset <- CAPR(cb).offset + GPR(rt);
        CAPR(cd) <- new_cap
    }

-----------------------------------
-- CSetLen
-----------------------------------
define COP2 > CHERICOP2 > CSet > CSetLen (cd::reg, cb::reg, rt::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException_v(cd)
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if not CAPR(cb).tag then
        SignalCapException(capExcTag,cb)
    else if CAPR(cb).sealed then
        SignalCapException(capExcSeal,cb)
    else if GPR(rt) >+ CAPR(cb).length then
        SignalCapException(capExcLength,cb)
    else
    {
        var new_cap = CAPR(cb);
        new_cap.length <- GPR(rt);
        CAPR(cd) <- new_cap
    }

-----------------------------------
-- CClearTag
-----------------------------------
define COP2 > CHERICOP2 > CSet > CClearTag (cd::reg, cb::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException_v(cd)
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else
    {
        var new_cap = CAPR(cb);
        new_cap.tag <- false;
        CAPR(cd) <- new_cap
    }

-----------------------------------
-- CAndPerm
-----------------------------------
define COP2 > CHERICOP2 > CSet > CAndPerm (cd::reg, cb::reg, rt::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException_v(cd)
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if not CAPR(cb).tag then
        SignalCapException(capExcTag,cb)
    else if CAPR(cb).sealed then
        SignalCapException(capExcSeal,cb)
    else
    {
        var new_cap = CAPR(cb);
        new_cap.perms <- CAPR(cb).perms && GPR(rt)<30:0>;
        CAPR(cd) <- new_cap
    }

-----------------------------------
-- CSetOffset
-----------------------------------
define COP2 > CHERICOP2 > CSet > CSetOffset (cd::reg, cb::reg, rt::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException_v(cd)
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if CAPR(cb).tag and CAPR(cb).sealed then
        SignalCapException(capExcSeal,cb)
    else
    {
        var new_cap = CAPR(cb);
        new_cap.offset <- GPR(rt);
        CAPR(cd) <- new_cap
    }

-----------------------------------
-- CCheckPerm
-----------------------------------
define COP2 > CHERICOP2 > CCheck > CCheckPerm (cs::reg, rt::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cs) then
        SignalCapException_v(cs)
    else if not CAPR(cs).tag then
        SignalCapException(capExcTag,cs)
    else if CAPR(cs).perms && GPR(rt)<30:0> <> GPR(rt)<30:0> then
        SignalCapException(capExcUser,cs)
    else
        nothing

-----------------------------------
-- CChecType
-----------------------------------
define COP2 > CHERICOP2 > CCheck > CCheckType (cs::reg, cb::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cs) then
        SignalCapException_v(cs)
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if not CAPR(cs).tag then
        SignalCapException(capExcTag,cs)
    else if not CAPR(cb).tag then
        SignalCapException(capExcTag,cb)
    else if not CAPR(cs).sealed then
        SignalCapException(capExcSeal,cs)
    else if not CAPR(cb).sealed then
        SignalCapException(capExcSeal,cb)
    else if CAPR(cs).otype <> CAPR(cb).otype then
        SignalCapException(capExcType,cs)
    else
        nothing

-----------------------------------
-- CFromPtr
-----------------------------------
define COP2 > CHERICOP2 > CSet > CFromPtr (cd::reg, cb::reg, rt::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException_v(cd)
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if GPR(rt) == 0 then
    {
        var new_cap::Capability;
        new_cap.tag <- false;
        new_cap.sealed <- false;
        new_cap.perms <- 0;
        new_cap.base <- 0;
        new_cap.length <- 0;
        new_cap.offset <- 0;
        new_cap.otype <- 0;
        new_cap.reserved <- 0;
        CAPR(cd) <- new_cap
    }
    else if not CAPR(cb).tag then
        SignalCapException(capExcTag,cb)
    else if CAPR(cb).sealed then
        SignalCapException(capExcSeal,cb)
    else if GPR(rt) >+ CAPR(cb).length then
        SignalCapException(capExcLength,cb)
    else
    {
        var new_cap = CAPR(cb);
        new_cap.base <- CAPR(cb).base + GPR(rt);
        new_cap.length <- CAPR(cb).length - GPR(rt);
        CAPR(cd) <- new_cap
    }

-----------------------------------
-- CToPtr
-----------------------------------
define COP2 > CHERICOP2 > CGet > CToPtr (rd::reg, cb::reg, ct::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if register_inaccessible(ct) then
        SignalCapException_v(ct)
    else if not CAPR(ct).tag then
        SignalCapException(capExcTag,ct)
    else if not CAPR(cb).tag then
        GPR(rd) <- 0
    else
        GPR(rd) <- CAPR(cb).base + CAPR(cb).offset - CAPR(ct).base

-----------------------------------
-- CPtrCmp
-----------------------------------
define COP2 > CHERICOP2 > CPtrCmp (rd::reg, cb::reg, ct::reg, t::bits(3)) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else
    {
        var equal = false;
        var less;
        var greater;
        var lessu;
        var greateru;
        if register_inaccessible(cb) then
            SignalCapException_v(cb)
        else if register_inaccessible(ct) then
            SignalCapException_v(ct)
        else if CAPR(cb).tag <> CAPR(ct).tag then
            if CAPR(cb).tag then
            {
                less <- false;
                lessu <- false;
                greater <- true;
                greateru <- true
            }
            else
            {
                less <- true;
                lessu <- true;
                greater <- false;
                greateru <- false
            }
        else
        {
            cursor1 = CAPR(cb).base + CAPR(cb).offset; -- mod 2^64
            cursor2 = CAPR(ct).base + CAPR(ct).offset; -- mod 2^64
            equal <- cursor1 == cursor2;
            less <- cursor1 < cursor2;
            greater <- cursor1 > cursor2;
            lessu <- cursor1 <+ cursor2;
            greateru <- cursor1 >+ cursor2
        };
        match t
        {
           case 0 => GPR(rd) <- [equal]
           case 1 => GPR(rd) <- [not equal]
           case 2 => GPR(rd) <- [less]
           case 3 => GPR(rd) <- [less or equal]
           case 4 => GPR(rd) <- [lessu]
           case 5 => GPR(rd) <- [lessu or equal]
           case _ => nothing
        }
    }

-----------------------------------
-- CBTU
-----------------------------------
define COP2 > CHERICOP2 > CBTU (cb::reg, offset::bits(16)) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else
    {
        CheckBranch;
        if register_inaccessible(cb) then
            SignalCapException_v(cb)
        else if not CAPR(cb).tag then
            if PC + SignExtend(offset) + 4 >+ PCC.length then
                SignalCapException_noReg(capExcLength)
            else
                BranchTo <- Some (PC + 4 + SignExtend(offset) << 2)
        else
            nothing
    }

-----------------------------------
-- CBTS
-----------------------------------
define COP2 > CHERICOP2 > CBTS (cb::reg, offset::bits(16)) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else
    {
        CheckBranch;
        if register_inaccessible(cb) then
            SignalCapException_v(cb)
        else if CAPR(cb).tag then
            if PC + SignExtend(offset) + 4 >+ PCC.length then
                SignalCapException_noReg(capExcLength)
            else
                BranchTo <- Some (PC + 4 + SignExtend(offset) << 2)
        else
            nothing
    }

-----------------------------------
-- CSC
-----------------------------------
define SDC2 > CHERISDC2 > CSC (cs::reg, cb::reg, rt::reg, offset::bits(11)) =
    if register_inaccessible(cs) then
        SignalCapException_v(cs)
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if not CAPR(cb).tag then
        SignalCapException(capExcTag,cb)
    else if CAPR(cb).sealed then
        SignalCapException(capExcSeal,cb)
    else if not Perms(CAPR(cb).perms).Permit_Store_Capability then
        SignalCapException(capExcPermStoreCap,cb)
    else if not Perms(CAPR(cb).perms).Permit_Store_Local_Capability
            and CAPR(cs).tag and not Perms(CAPR(cs).perms).Global then
        SignalCapException(capExcPermStoreLocalCap,cb)
    else
    {
        cursor = CAPR(cb).base + CAPR(cb).offset; -- mod 2^64
        addr = cursor + GPR(rt) + SignExtend(offset);
        if ('0':GPR(rt)) + SignExtend(offset) + 32 >+ ('0':CAPR(cb).length) then
            SignalCapException(capExcLength,cb)
        else if ('0':GPR(rt)) + SignExtend(offset) < 0 then
            SignalCapException(capExcLength,cb)
        else if addr<4:0> <> '00000' then
        {
            CP0.BadVAddr <- addr;
            SignalException(AdES)
        }
        else
        {
            StoreCap(addr,CAPR(cs));
            LLbit <- None
        }
    }

-----------------------------------
-- CLC
-----------------------------------
define LDC2 > CHERILDC2 > CLC (cd::reg, cb::reg, rt::reg, offset::bits(11)) =
    if register_inaccessible(cd) then
        SignalCapException_v(cd)
    else if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if not CAPR(cb).tag then
        SignalCapException(capExcTag,cb)
    else if CAPR(cb).sealed then
        SignalCapException(capExcSeal,cb)
    else if not Perms(CAPR(cb).perms).Permit_Load_Capability then
        SignalCapException(capExcPermLoadCap,cb)
    else
    {
        cursor = CAPR(cb).base + CAPR(cb).offset; -- mod 2^64
        addr = cursor + GPR(rt) + SignExtend(offset);
        if ('0':GPR(rt)) + SignExtend(offset) + 32 >+ ('0':CAPR(cb).length) then
            SignalCapException(capExcLength,cb)
        else if ('0':GPR(rt)) + SignExtend(offset) < 0 then
            SignalCapException(capExcLength,cb)
        else if addr<4:0> <> '00000' then
        {
            CP0.BadVAddr <- addr;
            SignalException(AdEL)
        }
        else
        {
            tmp = LoadCap(addr);
            when not exceptionSignalled do CAPR(cd) <- tmp;
            LLbit <- None
        }
    }

-----------------------------------
-- CLoad
-----------------------------------
define LWC2 > CHERILWC2 > CLoad (rd::reg, cb::reg, rt::reg, offset::bits(8), s::bits(1), t::bits(2)) =
    if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if not CAPR(cb).tag then
        SignalCapException(capExcTag,cb)
    else if CAPR(cb).sealed then
        SignalCapException(capExcSeal,cb)
    else if not Perms(CAPR(cb).perms).Permit_Load then
        SignalCapException(capExcPermLoad,cb)
    else
    {
        var access;
        var size;
        var aligned;
        cursor = CAPR(cb).base + CAPR(cb).offset; -- mod 2^64 ?
        var addr = cursor + GPR(rt) + SignExtend(offset);
        var bytesel = '000';
        match t
        {
            case 0 =>
            {
                size    <- 1;
                access  <- BYTE;
                bytesel <- addr<2:0> ?? BigEndianCPU^3;
                aligned <- true
            }
            case 1 =>
            {
                size    <- 2;
                access  <- HALFWORD;
                bytesel <- addr<2:0> ?? (BigEndianCPU^2 : '0');
                aligned <- addr<0> == false
            }
            case 2 =>
            {
                size    <- 4;
                access  <- WORD;
                bytesel <- addr<2:0> ?? (BigEndianCPU : '00');
                aligned <- addr<1:0> == 0
            }
            case 3 =>
            {
                size    <- 8;
                access  <- DOUBLEWORD;
                aligned <- addr<2:0> == 0
            }
        };
        if SignExtend(offset) + ('0':GPR(rt)) + size >+ ('0':CAPR(cb).length) then
            SignalCapException(capExcLength,cb)
        else if SignExtend(offset) + ('0':GPR(rt)) < 0 then
            SignalCapException(capExcLength,cb)
        else if not aligned then
        {
            CP0.BadVAddr <- addr;
            SignalException(AdEL)
        }
        else
        {
            data = LoadMemoryCap(access, addr, DATA, LOAD, false);
            when not exceptionSignalled do
            {
                data_list = [data]::bool list;
                bottom = ([bytesel]::nat)*8;
                top = ([bytesel]::nat)*8 + ([size]::nat)*8 - 1;
                final_data = data_list<top:bottom>;
                if s == 0 then GPR(rd) <- [final_data]
                else GPR(rd) <- [SignExtendBitString(64, final_data)]
            }
        }
    }

-----------------------------------
-- CLLD
-----------------------------------
define LWC2 > CHERILWC2 > CLLD (rd::reg, cb::reg, rt::reg, offset::bits(8)) =
{
    cursor = CAPR(cb).base + CAPR(cb).offset; -- mod 2^64 ?
    addr = cursor + GPR(rt) + SignExtend(offset);
    if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if not CAPR(cb).tag then
        SignalCapException(capExcTag,cb)
    else if CAPR(cb).sealed then
        SignalCapException(capExcSeal,cb)
    else if not Perms(CAPR(cb).perms).Permit_Load then
        SignalCapException(capExcPermLoad,cb)
    else if SignExtend(offset) + ('0':GPR(rt)) + 8 >+ ('0':CAPR(cb).length) then
        SignalCapException(capExcLength,cb)
    else if SignExtend(offset) + ('0':GPR(rt)) < 0 then
        SignalCapException(capExcLength,cb)
    else if addr<3:0> <> 0 then
    {
        CP0.BadVAddr <- addr;
        SignalException(AdEL)
    }
    else
        GPR(rd) <- LoadMemoryCap(DOUBLEWORD, addr, DATA, LOAD, true)
}

-----------------------------------
-- CStore
-----------------------------------
define SWC2 > CHERISWC2 > CStore (rs::reg, cb::reg, rt::reg, offset::bits(8), t::bits(2)) =
    if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if not CAPR(cb).tag then
        SignalCapException(capExcTag,cb)
    else if CAPR(cb).sealed then
        SignalCapException(capExcSeal,cb)
    else if not Perms(CAPR(cb).perms).Permit_Store then
        SignalCapException(capExcPermStore,cb)
    else
    {
        var access;
        var size;
        var aligned;
        cursor = CAPR(cb).base + CAPR(cb).offset; -- mod 2^64 ?
        var addr = cursor + GPR(rt) + SignExtend(offset);
        var bytesel = '000';
        match t
        {
            case 0 =>
            {
                size    <- 1;
                access  <- BYTE;
                bytesel <- addr<2:0> ?? BigEndianCPU^3;
                aligned <- true
            }
            case 1 =>
            {
                size    <- 2;
                access  <- HALFWORD;
                bytesel <- addr<2:0> ?? (BigEndianCPU^2:'0');
                aligned <- addr<0> == false
            }
            case 2 =>
            {
                size    <- 4;
                access  <- WORD;
                bytesel <- addr<2:0> ?? (BigEndianCPU^1:'00');
                aligned <- addr<1:0> == 0
            }
            case 3 =>
            {
                size    <- 8;
                access  <- DOUBLEWORD;
                aligned <- addr<2:0> == 0
            }
        };
        if SignExtend(offset) + ('0':GPR(rt)) + size >+ ('0':CAPR(cb).length) then
            SignalCapException(capExcLength,cb)
        else if SignExtend(offset) + ('0':GPR(rt)) < 0 then
            SignalCapException(capExcLength,cb)
        else if not aligned then
        {
            CP0.BadVAddr <- addr;
            SignalException(AdES)
        }
        else
        {
            _ = StoreMemoryCap(access, access, GPR(rs) << (0n8 * [bytesel]), addr, DATA, STORE, false);
            nothing
        }
    }

-----------------------------------
-- CSCD
-----------------------------------
define SWC2 > CHERISWC2 > CSCD (rs::reg, cb::reg, rt::reg, offset::bits(8)) =
{
    cursor = CAPR(cb).base + CAPR(cb).offset; -- mod 2^64 ?
    addr = cursor + GPR(rt) + SignExtend(offset);
    if register_inaccessible(cb) then
        SignalCapException_v(cb)
    else if not CAPR(cb).tag then
        SignalCapException(capExcTag,cb)
    else if CAPR(cb).sealed then
        SignalCapException(capExcSeal,cb)
    else if not Perms(CAPR(cb).perms).Permit_Store then
        SignalCapException(capExcPermStore,cb)
    else if SignExtend(offset) + ('0':GPR(rt)) + 32 >+ ('0':CAPR(cb).length) then
        SignalCapException(capExcLength,cb)
    else if SignExtend(offset) + ('0':GPR(rt)) < 0 then
        SignalCapException(capExcLength,cb)
    else if addr<4:0> <> 0 then
    {
        CP0.BadVAddr <- addr;
        SignalException(AdES)
    }
    else
        GPR(rs) <- if StoreMemoryCap(DOUBLEWORD, DOUBLEWORD, GPR(rs), addr, DATA, LOAD, true) then 1 else 0
}

-----------------------------------
-- CJR
-----------------------------------
define COP2 > CHERICOP2 > CJR (cb::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else
    {
        CheckBranch;
        if register_inaccessible(cb) then
            SignalCapException_v(cb)
        else if not CAPR(cb).tag then
            SignalCapException(capExcTag,cb)
        else if CAPR(cb).sealed then
            SignalCapException(capExcSeal,cb)
        else if not Perms(CAPR(cb).perms).Permit_Execute then
            SignalCapException(capExcPermExe,cb)
        else if not Perms(CAPR(cb).perms).Global then
            SignalCapException(capExcGlobal,cb)
        else if CAPR(cb).offset + 4 >+ CAPR(cb).length then
            SignalCapException(capExcLength,cb)
        else if (CAPR(cb).base + CAPR(cb).offset)<1:0> <> '00' then
        {
            CP0.BadVAddr <- (CAPR(cb).base + CAPR(cb).offset);
            SignalException(AdEL)
        }
        else
        {
            BranchToPCC <- Some (CAPR(cb).offset, CAPR(cb))
        }
    }

-----------------------------------
-- CJALR
-----------------------------------
define COP2 > CHERICOP2 > CJALR (cd::reg, cb::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else
    {
        CheckBranch;
        if register_inaccessible(cd) then
            SignalCapException_v(cd)
        else if register_inaccessible(cb) then
            SignalCapException_v(cb)
        else if not CAPR(cb).tag then
            SignalCapException(capExcTag,cb)
        else if CAPR(cb).sealed then
            SignalCapException(capExcSeal,cb)
        else if not Perms(CAPR(cb).perms).Permit_Execute then
            SignalCapException(capExcPermExe,cb)
        else if not Perms(CAPR(cb).perms).Global then
            SignalCapException(capExcGlobal,cb)
        else if CAPR(cb).offset + 4 >+ CAPR(cb).length then
            SignalCapException(capExcLength,cb)
        else if (CAPR(cb).base + CAPR(cb).offset)<1:0> <> '00' then
        {
            CP0.BadVAddr <- (CAPR(cb).base + CAPR(cb).offset);
            SignalException(AdEL)
        }
        else
        {
            var new_cap = PCC;
            new_cap.offset <- PC + 8;
            CAPR(cd) <- new_cap;
            BranchToPCC <- Some (CAPR(cb).offset, CAPR(cb))
        }
    }

-----------------------------------
-- CSeal
-----------------------------------
define COP2 > CHERICOP2 > CSeal (cd::reg, cs::reg, ct::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException_v(cd)
    else if register_inaccessible(cs) then
        SignalCapException_v(cs)
    else if register_inaccessible(ct) then
        SignalCapException_v(ct)
    else if not CAPR(cs).tag then
        SignalCapException(capExcTag,cs)
    else if not CAPR(ct).tag then
        SignalCapException(capExcTag,ct)
    else if CAPR(cs).sealed then
        SignalCapException(capExcSeal,cs)
    else if CAPR(ct).sealed then
        SignalCapException(capExcSeal,ct)
    else if not Perms(CAPR(ct).perms).Permit_Seal then
        SignalCapException(capExcPermSeal,ct)
    else if CAPR(ct).offset >=+ CAPR(ct).length then
        SignalCapException(capExcLength,ct)
    else if (CAPR(ct).base + CAPR(ct).offset) >=+ 0x0000000001000000 then
        SignalCapException(capExcLength,ct)
    else
    {
        var new_cap = CAPR(cs);
        new_cap.sealed <- true;
        new_cap.otype <- (CAPR(ct).base + CAPR(ct).offset)<23:0>;
        CAPR(cd) <- new_cap
    }

-----------------------------------
-- CUnseal
-----------------------------------
define COP2 > CHERICOP2 > CUnseal (cd::reg, cs::reg, ct::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else if register_inaccessible(cd) then
        SignalCapException_v(cd)
    else if register_inaccessible(cs) then
        SignalCapException_v(cs)
    else if register_inaccessible(ct) then
        SignalCapException_v(ct)
    else if not CAPR(cs).tag then
        SignalCapException(capExcTag,cs)
    else if not CAPR(ct).tag then
        SignalCapException(capExcTag,ct)
    else if not CAPR(cs).sealed then
        SignalCapException(capExcSeal,cs)
    else if CAPR(ct).sealed then
        SignalCapException(capExcSeal,ct)
    else if (CAPR(ct).base + CAPR(ct).offset)<23:0> <> CAPR(cs).otype then
        SignalCapException(capExcType,ct)
    else if not Perms(CAPR(ct).perms).Permit_Seal then
        SignalCapException(capExcPermSeal,ct)
    else if CAPR(ct).offset >=+ CAPR(ct).length then
        SignalCapException(capExcLength,ct)
    else
    {
        var new_cap = CAPR(cs);
        new_cap.sealed <- false;
        new_cap.otype <- 0;
        Perms(new_cap.perms).Global <- Perms(CAPR(cs).perms).Global and Perms(CAPR(ct).perms).Global;
        CAPR(cd) <- new_cap
    }

-----------------------------------
-- CCall
-----------------------------------
define COP2 > CHERICOP2 > CCall (cs::reg, cb::reg) =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else SignalCapException(capExcCall,cs)

-----------------------------------
-- CReturn
-----------------------------------
define COP2 > CHERICOP2 > CReturn =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
    else SignalCapException_noReg(capExcRet)

-------------------------------------------------------------
-- Unknown Capability Instruction, i.e. unsuccessful decode.
-------------------------------------------------------------
define COP2 > CHERICOP2 > UnknownCapInstruction =
    if not CP0.Status.CU2 then
        SignalCP2UnusableException
   else SignalException (ResI)
