---------------------------------------------------------------------------
-- Pretty printing stubs for CHERI instructions
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

string COP2InstructionToString (i::instruction) =
    match i
    {
        case COP2(CHERICOP2(j)) =>
            match j
            {
                case DumpCapReg                   => "mtc2 ?,?,6"
                case CGet(CGetBase(rd, cb))       => op2r("cgetbase",rd,cb)
                case CGet(CGetOffset(rd, cb))     => op2r("cgetoffset",rd,cb)
                case CGet(CGetLen(rd, cb))        => op2r("cgetlen",rd,cb)
                case CGet(CGetTag(rd, cb))        => op2r("cgettag",rd,cb)
                case CGet(CGetSealed(rd, cb))     => op2r("cgetsealed",rd,cb)
                case CGet(CGetPerm(rd, cb))       => op2r("cgetperm",rd,cb)
                case CGet(CGetType(rd, cb))       => op2r("cgettype",rd,cb)
                case CGet(CGetPCC(cd))            => op1r("cgetpcc",cd)
                case CGet(CGetCause(rd))          => op1r("cgetcause",rd)
                case CSet(CSetCause(rt))          => op1r("csetcause",rt)
                case CSet(CIncBase(cd, cb, rt))   => op3r("cincbase",cd,cb,rt)
                case CSet(CIncOffset(cd, cb, rt)) => op3r("cincoffset",cd,cb,rt)
                case CSet(CSetLen(cd, cb, rt))    => op3r("csetlen",cd,cb,rt)
                case CSet(CClearTag(cd, cb))      => op2r("ccleartag",cd,cb)
                case CSet(CAndPerm(cd, cb, rt))   => op3r("candperm",cd,cb,rt)
                case CSet(CSetOffset(cd, cb, rt)) => op3r("csetoffset",cd,cb,rt)
                case CCheck(CCheckPerm(cs, rt))   => op2r("ccheckperm",cs,rt)
                case CCheck(CCheckType(cs, cb))   => op2r("cchecktype",cs,cb)
                case CSet(CFromPtr(cd, cb, rt))   => op3r("cfromptr",cd,cb,rt)
                case CGet(CToPtr(rd, cb, ct))     => op3r("ctoptr",rd,cb,ct)
                case CPtrCmp(rd, cb, ct, t)       =>
                    match t
                    {
                        case 0b000 => op3r("ceq",rd,cb,ct)
                        case 0b001 => op3r("cne",rd,cb,ct)
                        case 0b010 => op3r("clt",rd,cb,ct)
                        case 0b011 => op3r("cle",rd,cb,ct)
                        case 0b100 => op3r("cltu",rd,cb,ct)
                        case 0b101 => op3r("cleu",rd,cb,ct)
                        case _     => "unmatched_cap_inst"
                    }
                case CBTU(cb, offset)             => op1ri("cbtu",cb,offset)
                case CBTS(cb, offset)             => op1ri("cbts",cb,offset)
                case CJR(cb)                      => op1r("cjr",cb)
                case CJALR(cd, cb)                => op2r("cjalr",cd,cb)
                case CSeal(cd, cs, ct)            => op3r("cseal",cd,cs,ct)
                case CUnseal(cd, cs, ct)          => op3r("cunseal",cd,cs,ct)
                case CCall(cs, cb)                => op2r("ccall",cs,cb)
                case CReturn                      => "creturn"
                case UnknownCapInstruction        => "unknown_cap_inst"
            }
        case _ => "unmatched_cap_inst"
    }

string LWC2InstructionToString (i::instruction) =
    match i
    {
        case LWC2(CHERILWC2(j)) =>
            match j
            {
                case CLoad(rd, cb, rt, offset, 0b0, t) =>
                    match t
                    {
                        case 0b00 => op3ro("clbu",rd,cb,rt,offset)
                        case 0b01 => op3ro("clhu",rd,cb,rt,offset)
                        case 0b10 => op3ro("clwu",rd,cb,rt,offset)
                        case 0b11 => op3ro("cld",rd,cb,rt,offset)
                    }
                case CLoad(rd, cb, rt, offset, 0b1, 0b00) => op3ro("clb",rd,cb,rt,offset)
                case CLoad(rd, cb, rt, offset, 0b1, 0b01) => op3ro("clh",rd,cb,rt,offset)
                case CLoad(rd, cb, rt, offset, 0b1, 0b10) => op3ro("clw",rd,cb,rt,offset)
                case CLLD(rd, cb, rt, offset)             => op3ro("clld",rd,cb,rt,offset)
                case _                                    => "unmatched_cap_inst"
            }
        case _ => "unmatched_cap_inst"
    }

string LDC2InstructionToString (i::instruction) =
    match i
    {
        case LDC2(CHERILDC2(j)) =>
            match j
            {
                case CLC(cd, cb, rt, offset) => op3ro("clc",cd,cb,rt,offset)
            }
        case _ => "unmatched_cap_inst"
    }

string SWC2InstructionToString (i::instruction) =
    match i
    {
        case SWC2(CHERISWC2(j)) =>
            match j
            {
                case CStore(rs, cb, rt, offset, t) =>
                    match t
                    {
                        case 0b00 => op3ro("csb",rs,cb,rt,offset)
                        case 0b01 => op3ro("csh",rs,cb,rt,offset)
                        case 0b10 => op3ro("csw",rs,cb,rt,offset)
                        case 0b11 => op3ro("csd",rs,cb,rt,offset)
                    }
                case CSCD(rs, cb, rt, offset) => op3ro("cscd",rs,cb,rt,offset)
            }
        case _ => "unmatched_cap_inst"
    }

string SDC2InstructionToString (i::instruction) =
    match i
    {
        case SDC2(CHERISDC2(j)) =>
            match j
            {
                case CSC(cs, cb, rt, offset) => op3ro("csc",cs,cb,rt,offset)
            }
        case _ => "unmatched_cap_inst"
    }

word COP2Encode (i::instruction) = '010010' : 0
word LWC2Encode (i::instruction) = '110010' : 0
word LDC2Encode (i::instruction) = '110110' : 0
word SWC2Encode (i::instruction) = '111010' : 0
word SDC2Encode (i::instruction) = '111110' : 0
