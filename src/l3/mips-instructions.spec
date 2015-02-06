---------------------------------------------------------------------------
-- The standard MIPS instructions implementation
-- (c) Anthony Fox, University of Cambridge
---------------------------------------------------------------------------

-----------------------------------
-- ADDI rt, rs, immediate
-----------------------------------
define ArithI > ADDI (rs::reg, rt::reg, immediate::bits(16)) =
{
   when NotWordValue (GPR(rs)) do #UNPREDICTABLE("ADDI: NotWordValue");
   temp = GPR(rs)<32:0> + SignExtend (immediate);
   if temp<32> <> temp<31> then
      SignalException (Ov)
   else
      GPR(rt) <- SignExtend (temp<31:0>)
}

-----------------------------------
-- ADDIU rt, rs, immediate
-----------------------------------
define ArithI > ADDIU (rs::reg, rt::reg, immediate::bits(16)) =
{
   when NotWordValue (GPR(rs)) do #UNPREDICTABLE("ADDIU: NotWordValue");
   temp = GPR(rs)<31:0> + SignExtend (immediate);
   GPR(rt) <- SignExtend (temp)
}

-----------------------------------
-- DADDI rt, rs, immediate
-----------------------------------
define ArithI > DADDI (rs::reg, rt::reg, immediate::bits(16)) =
{
   temp`65 = SignExtend (GPR(rs)) + SignExtend (immediate);
   if temp<64> <> temp<63> then
      SignalException (Ov)
   else
      GPR(rt) <- temp<63:0>
}

-----------------------------------
-- DADDIU rt, rs, immediate
-----------------------------------
define ArithI > DADDIU (rs::reg, rt::reg, immediate::bits(16)) =
   GPR(rt) <- GPR(rs) + SignExtend (immediate)

-----------------------------------
-- SLTI rt, rs, immediate
-----------------------------------
define ArithI > SLTI (rs::reg, rt::reg, immediate::bits(16)) =
   GPR(rt) <- [GPR(rs) < SignExtend (immediate)]

-----------------------------------
-- SLTIU rt, rs, immediate
-----------------------------------
define ArithI > SLTIU (rs::reg, rt::reg, immediate::bits(16)) =
   GPR(rt) <- [GPR(rs) <+ SignExtend (immediate)]

-----------------------------------
-- ANDI rt, rs, immediate
-----------------------------------
define ArithI > ANDI (rs::reg, rt::reg, immediate::bits(16)) =
   GPR(rt) <- GPR(rs) && ZeroExtend (immediate)

-----------------------------------
-- ORI rt, rs, immediate
-----------------------------------
define ArithI > ORI (rs::reg, rt::reg, immediate::bits(16)) =
   GPR(rt) <- GPR(rs) || ZeroExtend (immediate)

-----------------------------------
-- XORI rt, rs, immediate
-----------------------------------
define ArithI > XORI (rs::reg, rt::reg, immediate::bits(16)) =
   GPR(rt) <- GPR(rs) ?? ZeroExtend (immediate)

-----------------------------------
-- LUI rt, immediate
-----------------------------------
define ArithI > LUI (rt::reg, immediate::bits(16)) =
   GPR(rt) <- SignExtend (immediate : 0`16)

-----------------------------------
-- ADD rd, rs, rt
-----------------------------------
define ArithR > ADD (rs::reg, rt::reg, rd::reg) =
{
   when NotWordValue (GPR(rs)) or NotWordValue (GPR(rt))
     do #UNPREDICTABLE("ADD: NotWordValue");
   temp = GPR(rs)<32:0> + GPR(rt)<32:0>;
   if temp<32> <> temp<31> then
      SignalException (Ov)
   else
      GPR(rd) <- SignExtend (temp<31:0>)
}

-----------------------------------
-- ADDU rd, rs, rt
-----------------------------------
define ArithR > ADDU (rs::reg, rt::reg, rd::reg) =
{
   when NotWordValue (GPR(rs)) or NotWordValue (GPR(rt))
     do #UNPREDICTABLE("ADDU: NotWordValue");
   temp = GPR(rs)<31:0> + GPR(rt)<31:0>;
   GPR(rd) <- SignExtend (temp)
}

-----------------------------------
-- SUB rd, rs, rt
-----------------------------------
define ArithR > SUB (rs::reg, rt::reg, rd::reg) =
{
   when NotWordValue (GPR(rs)) or NotWordValue (GPR(rt))
     do #UNPREDICTABLE("SUB: NotWordValue");
   temp = GPR(rs)<32:0> - GPR(rt)<32:0>;
   if temp<32> <> temp<31> then
      SignalException (Ov)
   else
      GPR(rd) <- SignExtend (temp<31:0>)
}

-----------------------------------
-- SUBU rd, rs, rt
-----------------------------------
define ArithR > SUBU (rs::reg, rt::reg, rd::reg) =
{
   when NotWordValue (GPR(rs)) or NotWordValue (GPR(rt))
     do #UNPREDICTABLE("SUBU: NotWordValue");
   temp = GPR(rs)<31:0> - GPR(rt)<31:0>;
   GPR(rd) <- SignExtend (temp)
}

-----------------------------------
-- DADD rd, rs, rt
-----------------------------------
define ArithR > DADD (rs::reg, rt::reg, rd::reg) =
{
   temp`65 = SignExtend (GPR(rs)) + SignExtend (GPR(rt));
   if temp<64> <> temp<63> then
      SignalException (Ov)
   else
      GPR(rd) <- temp<63:0>
}

-----------------------------------
-- DADDU rd, rs, rt
-----------------------------------
define ArithR > DADDU (rs::reg, rt::reg, rd::reg) =
   GPR(rd) <- GPR(rs) + GPR(rt)

-----------------------------------
-- DSUB rd, rs, rt
-----------------------------------
define ArithR > DSUB (rs::reg, rt::reg, rd::reg) =
{
   temp`65 = SignExtend (GPR(rs)) - SignExtend (GPR(rt));
   if temp<64> <> temp<63> then
      SignalException (Ov)
   else
      GPR(rd) <- temp<63:0>
}

-----------------------------------
-- DSUBU rd, rs, rt
-----------------------------------
define ArithR > DSUBU (rs::reg, rt::reg, rd::reg) =
   GPR(rd) <- GPR(rs) - GPR(rt)

-----------------------------------
-- SLT rd, rs, rt
-----------------------------------
define ArithR > SLT (rs::reg, rt::reg, rd::reg) =
   GPR(rd) <- [GPR(rs) < GPR(rt)]

-----------------------------------
-- SLTU rd, rs, rt
-----------------------------------
define ArithR > SLTU (rs::reg, rt::reg, rd::reg) =
   GPR(rd) <- [GPR(rs) <+ GPR(rt)]

-----------------------------------
-- AND rd, rs, rt
-----------------------------------
define ArithR > AND (rs::reg, rt::reg, rd::reg) =
   GPR(rd) <- GPR(rs) && GPR(rt)

-----------------------------------
-- OR rd, rs, rt
-----------------------------------
define ArithR > OR (rs::reg, rt::reg, rd::reg) =
   GPR(rd) <- GPR(rs) || GPR(rt)

-----------------------------------
-- XOR rd, rs, rt
-----------------------------------
define ArithR > XOR (rs::reg, rt::reg, rd::reg) =
   GPR(rd) <- GPR(rs) ?? GPR(rt)

-----------------------------------
-- NOR rd, rs, rt
-----------------------------------
define ArithR > NOR (rs::reg, rt::reg, rd::reg) =
   GPR(rd) <- ~(GPR(rs) || GPR(rt))

-----------------------------------
-- MOVN rd, rs, rt
-----------------------------------
define ArithR > MOVN (rs::reg, rt::reg, rd::reg) =
   when GPR(rt) <> 0 do
      GPR(rd) <- GPR(rs)

-----------------------------------
-- MOVZ rd, rs, rt
-----------------------------------
define ArithR > MOVZ (rs::reg, rt::reg, rd::reg) =
   when GPR(rt) == 0 do
      GPR(rd) <- GPR(rs)

-----------------------------------
-- MADD rs, rt
-----------------------------------
define MultDiv > MADD (rs::reg, rt::reg) =
{
   when NotWordValue (GPR(rs)) or NotWordValue (GPR(rt))
     do #UNPREDICTABLE("MADD: NotWordValue");
   temp`64 = (HI<31:0> : LO<31:0>) +
             SignExtend (GPR(rs)<31:0>) * SignExtend (GPR(rt)<31:0>);
   HI <- SignExtend (temp<63:32>);
   LO <- SignExtend (temp<31:0>)
}

-----------------------------------
-- MADDU rs, rt
-----------------------------------
define MultDiv > MADDU (rs::reg, rt::reg) =
{
   when NotWordValue (GPR(rs)) or NotWordValue (GPR(rt))
     do #UNPREDICTABLE("MADDU: NotWordValue");
   temp`64 = (HI<31:0> : LO<31:0>) +
             ZeroExtend (GPR(rs)<31:0>) * ZeroExtend (GPR(rt)<31:0>);
   HI <- SignExtend (temp<63:32>);
   LO <- SignExtend (temp<31:0>)
}

-----------------------------------
-- MSUB rs, rt
-----------------------------------
define MultDiv > MSUB (rs::reg, rt::reg) =
{
   when NotWordValue (GPR(rs)) or NotWordValue (GPR(rt))
     do #UNPREDICTABLE("MSUB: NotWordValue");
   temp`64 = (HI<31:0> : LO<31:0>) -
             SignExtend (GPR(rs)<31:0>) * SignExtend (GPR(rt)<31:0>);
   HI <- SignExtend (temp<63:32>);
   LO <- SignExtend (temp<31:0>)
}

-----------------------------------
-- MSUBU rs, rt
-----------------------------------
define MultDiv > MSUBU (rs::reg, rt::reg) =
{
   when NotWordValue (GPR(rs)) or NotWordValue (GPR(rt))
     do #UNPREDICTABLE("MSUBU: NotWordValue");
   temp`64 = (HI<31:0> : LO<31:0>) -
             ZeroExtend (GPR(rs)<31:0>) * ZeroExtend (GPR(rt)<31:0>);
   HI <- SignExtend (temp<63:32>);
   LO <- SignExtend (temp<31:0>)
}

-----------------------------------
-- MUL rd, rs, rt
-----------------------------------
define MultDiv > MUL (rs::reg, rt::reg, rd::reg) =
{
   when NotWordValue (GPR(rs)) or NotWordValue (GPR(rt))
     do #UNPREDICTABLE("MUL: NotWordValue");
   GPR(rd) <- SignExtend (GPR(rs)<31:0> * GPR(rt)<31:0>);
   lo <- None;
   hi <- None
}

-----------------------------------
-- MULT rs, rt
-----------------------------------
define MultDiv > MULT (rs::reg, rt::reg) =
{
   when NotWordValue (GPR(rs)) or NotWordValue (GPR(rt))
     do #UNPREDICTABLE("MULT: NotWordValue");
   prod`64 = SignExtend (GPR(rs)<31:0>) * SignExtend (GPR(rt)<31:0>);
   LO <- SignExtend (prod<31:0>);
   HI <- SignExtend (prod<63:32>)
}

-----------------------------------
-- MULTU rs, rt
-----------------------------------
define MultDiv > MULTU (rs::reg, rt::reg) =
{
   when NotWordValue (GPR(rs)) or NotWordValue (GPR(rt))
     do #UNPREDICTABLE("MULTU: NotWordValue");
   prod`64 = ZeroExtend (GPR(rs)<31:0>) * ZeroExtend (GPR(rt)<31:0>);
   LO <- SignExtend (prod<31:0>);
   HI <- SignExtend (prod<63:32>)
}

-----------------------------------
-- DMULT rs, rt
-----------------------------------
define MultDiv > DMULT (rs::reg, rt::reg) =
{
   prod`128 = SignExtend (GPR(rs)) * SignExtend (GPR(rt));
   LO <- prod<63:0>;
   HI <- prod<127:64>
}

-----------------------------------
-- DMULTU rs, rt
-----------------------------------
define MultDiv > DMULTU (rs::reg, rt::reg) =
{
   prod`128 = ZeroExtend (GPR(rs)) * ZeroExtend (GPR(rt));
   LO <- prod<63:0>;
   HI <- prod<127:64>
}

-----------------------------------
-- DIV rs, rt
-----------------------------------
define MultDiv > DIV (rs::reg, rt::reg) =
{
   s = GPR(rs);
   t = GPR(rt);
   when NotWordValue (s) or NotWordValue (t)
     do #UNPREDICTABLE("DIV: NotWordValue");
   if t == 0 then
   {
      lo <- None;
      hi <- None
   }
   else
   {
      q = s<31:0> quot t<31:0>;
      r = s<31:0> rem t<31:0>;
      LO <- SignExtend (q);
      HI <- SignExtend (r)
   }
}

-----------------------------------
-- DIVU rs, rt
-----------------------------------
define MultDiv > DIVU (rs::reg, rt::reg) =
{
   s = GPR(rs);
   t = GPR(rt);
   when NotWordValue (s) or NotWordValue (t)
     do #UNPREDICTABLE("DIVU: NotWordValue");
   if t == 0 then
   {
      lo <- None;
      hi <- None
   }
   else
   {
      q = s<31:0> div t<31:0>;
      r = s<31:0> mod t<31:0>;
      LO <- SignExtend (q);
      HI <- SignExtend (r)
   }
}

-----------------------------------
-- DDIV rs, rt
-----------------------------------
define MultDiv > DDIV (rs::reg, rt::reg) =
{
   t = GPR(rt);
   if t == 0 then
   {
      lo <- None;
      hi <- None
   }
   else
   {
      s = GPR(rs);
      LO <- s quot t;
      HI <- s rem t
   }
}

-----------------------------------
-- DDIVU rs, rt
-----------------------------------
define MultDiv > DDIVU (rs::reg, rt::reg) =
{
   t = GPR(rt);
   if t == 0 then
   {
      lo <- None;
      hi <- None
   }
   else
   {
      s = GPR(rs);
      LO <- s div t;
      HI <- s mod t
   }
}

-----------------------------------
-- MFHI rd
-----------------------------------
define MultDiv > MFHI (rd::reg) =
   GPR(rd) <- HI

-----------------------------------
-- MFLO rd
-----------------------------------
define MultDiv > MFLO (rd::reg) =
   GPR(rd) <- LO

-----------------------------------
-- MTHI rs
-----------------------------------
define MultDiv > MTHI (rs::reg) =
   HI <- GPR(rs)

-----------------------------------
-- MTLO rs
-----------------------------------
define MultDiv > MTLO (rs::reg) =
   LO <- GPR(rs)

-----------------------------------
-- SLL rd, rt, sa
-----------------------------------
define Shift > SLL (rt::reg, rd::reg, sa::bits(5)) =
   GPR(rd) <- SignExtend (GPR(rt)<31:0> << [sa])

-----------------------------------
-- SRL rd, rt, sa
-----------------------------------
define Shift > SRL (rt::reg, rd::reg, sa::bits(5)) =
{
   when NotWordValue (GPR(rt)) do #UNPREDICTABLE("SRL: NotWordValue");
   GPR(rd) <- SignExtend (GPR(rt)<31:0> >>+ [sa])
}

-----------------------------------
-- SRA rd, rt, sa
-----------------------------------
define Shift > SRA (rt::reg, rd::reg, sa::bits(5)) =
{
   when NotWordValue (GPR(rt)) do #UNPREDICTABLE("SRA: NotWordValue");
   GPR(rd) <- SignExtend (GPR(rt)<31:0> >> [sa])
}

-----------------------------------
-- SLLV rd, rt, rs
-----------------------------------
define Shift > SLLV (rs::reg, rt::reg, rd::reg) =
{
   sa = GPR(rs)<4:0>;
   GPR(rd) <- SignExtend (GPR(rt)<31:0> << [sa])
}

-----------------------------------
-- SRLV rd, rt, rs
-----------------------------------
define Shift > SRLV (rs::reg, rt::reg, rd::reg) =
{
   when NotWordValue (GPR(rt)) do #UNPREDICTABLE("SRLV: NotWordValue");
   sa = GPR(rs)<4:0>;
   GPR(rd) <- SignExtend (GPR(rt)<31:0> >>+ [sa])
}

-----------------------------------
-- SRAV rd, rt, rs
-----------------------------------
define Shift > SRAV (rs::reg, rt::reg, rd::reg) =
{
   when NotWordValue (GPR(rt)) do #UNPREDICTABLE("SRAV: NotWordValue");
   sa = GPR(rs)<4:0>;
   GPR(rd) <- SignExtend (GPR(rt)<31:0> >> [sa])
}

-----------------------------------
-- DSLL rd, rt, sa
-----------------------------------
define Shift > DSLL (rt::reg, rd::reg, sa::bits(5)) =
   GPR(rd) <- GPR(rt) << [sa]

-----------------------------------
-- DSRL rd, rt, sa
-----------------------------------
define Shift > DSRL (rt::reg, rd::reg, sa::bits(5)) =
   GPR(rd) <- GPR(rt) >>+ [sa]

-----------------------------------
-- DSRA rd, rt, sa
-----------------------------------
define Shift > DSRA (rt::reg, rd::reg, sa::bits(5)) =
   GPR(rd) <- GPR(rt) >> [sa]

-----------------------------------
-- DSLLV rd, rt, rs
-----------------------------------
define Shift > DSLLV (rs::reg, rt::reg, rd::reg) =
{
   sa = GPR(rs)<5:0>;
   GPR(rd) <- GPR(rt) << [sa]
}

-----------------------------------
-- DSRLV rd, rt, rs
-----------------------------------
define Shift > DSRLV (rs::reg, rt::reg, rd::reg) =
{
   sa = GPR(rs)<5:0>;
   GPR(rd) <- GPR(rt) >>+ [sa]
}

-----------------------------------
-- DSRAV rd, rt, rs
-----------------------------------
define Shift > DSRAV (rs::reg, rt::reg, rd::reg) =
{
   sa = GPR(rs)<5:0>;
   GPR(rd) <- GPR(rt) >> [sa]
}

-----------------------------------
-- DSLL32 rd, rt, sa
-----------------------------------
define Shift > DSLL32 (rt::reg, rd::reg, sa::bits(5)) =
   GPR(rd) <- GPR(rt) << ([sa] + 0n32)

-----------------------------------
-- DSRL32 rd, rt, sa
-----------------------------------
define Shift > DSRL32 (rt::reg, rd::reg, sa::bits(5)) =
   GPR(rd) <- GPR(rt) >>+ ([sa] + 0n32)

-----------------------------------
-- DSRA32 rd, rt, sa
-----------------------------------
define Shift > DSRA32 (rt::reg, rd::reg, sa::bits(5)) =
   GPR(rd) <- GPR(rt) >> ([sa] + 0n32)

-----------------------------------
-- TGE rs, rt
-----------------------------------
define Trap > TGE (rs::reg, rt::reg) =
   when GPR(rs) >= GPR(rt) do SignalException (Tr)

-----------------------------------
-- TGEU rs, rt
-----------------------------------
define Trap > TGEU (rs::reg, rt::reg) =
   when GPR(rs) >=+ GPR(rt) do SignalException (Tr)

-----------------------------------
-- TLT rs, rt
-----------------------------------
define Trap > TLT (rs::reg, rt::reg) =
   when GPR(rs) < GPR(rt) do SignalException (Tr)

-----------------------------------
-- TLTU rs, rt
-----------------------------------
define Trap > TLTU (rs::reg, rt::reg) =
   when GPR(rs) <+ GPR(rt) do SignalException (Tr)

-----------------------------------
-- TEQ rs, rt
-----------------------------------
define Trap > TEQ (rs::reg, rt::reg) =
   when GPR(rs) == GPR(rt) do SignalException (Tr)

-----------------------------------
-- TNE rs, rt
-----------------------------------
define Trap > TNE (rs::reg, rt::reg) =
   when GPR(rs) <> GPR(rt) do SignalException (Tr)

-----------------------------------
-- TGEI rs, immediate
-----------------------------------
define Trap > TGEI (rs::reg, immediate::bits(16)) =
   when GPR(rs) >= SignExtend (immediate) do SignalException (Tr)

-----------------------------------
-- TGEIU rs, immediate
-----------------------------------
define Trap > TGEIU (rs::reg, immediate::bits(16)) =
   when GPR(rs) >=+ SignExtend (immediate) do SignalException (Tr)

-----------------------------------
-- TLTI rs, immediate
-----------------------------------
define Trap > TLTI (rs::reg, immediate::bits(16)) =
   when GPR(rs) < SignExtend (immediate) do SignalException (Tr)

-----------------------------------
-- TLTIU rs, immediate
-----------------------------------
define Trap > TLTIU (rs::reg, immediate::bits(16)) =
   when GPR(rs) <+ SignExtend (immediate) do SignalException (Tr)

-----------------------------------
-- TEQI rs, immediate
-----------------------------------
define Trap > TEQI (rs::reg, immediate::bits(16)) =
   when GPR(rs) == SignExtend (immediate) do SignalException (Tr)

-----------------------------------
-- TNEI rs, immediate
-----------------------------------
define Trap > TNEI (rs::reg, immediate::bits(16)) =
   when GPR(rs) <> SignExtend (immediate) do SignalException (Tr)

-----------------------------------
-- LB  rt, offset(base)
-- LBU rt, offset(base)
-- LH  rt, offset(base)
-- LHU rt, offset(base)
-- LW  rt, offset(base)
-- LWU rt, offset(base)
-- LL  rt, offset(base)
-- LD  rt, offset(base)
-- LDU rt, offset(base)
-- LLD rt, offset(base)
-----------------------------------
unit loadByte (base::reg, rt::reg, offset::bits(16), unsigned::bool) =
{
   vAddr = SignExtend (offset) + GPR(base);
   memdoubleword = LoadMemory (BYTE, BYTE, vAddr, DATA, LOAD, false);
   when not exceptionSignalled do
   {
      byte = vAddr<2:0> ?? BigEndianCPU^3;
      membyte`8 = memdoubleword <7 + 8 * [byte] : 8 * [byte]>;
      GPR(rt) <- if unsigned then ZeroExtend (membyte)
                 else SignExtend (membyte)
   }
}

unit loadHalf (base::reg, rt::reg, offset::bits(16), unsigned::bool) =
{
   vAddr = SignExtend (offset) + GPR(base);
   if vAddr<0> then
   {
      CP0.BadVAddr <- vAddr;
      SignalException (AdEL)
   }
   else
   {
      memdoubleword = LoadMemory (HALFWORD, HALFWORD, vAddr, DATA, LOAD, false);
      when not exceptionSignalled do
      {
         byte = vAddr<2:0> ?? (BigEndianCPU^2 : '0');
         memhalf`16 = memdoubleword <15 + 8 * [byte] : 8 * [byte]>;
         GPR(rt) <- if unsigned then
                        ZeroExtend (memhalf)
                     else
                        SignExtend (memhalf)
      }
   }
}

unit loadWord (link::bool, base::reg, rt::reg, offset::bits(16),
               unsigned::bool) =
{
   vAddr = SignExtend (offset) + GPR(base);
   if vAddr<1:0> <> '00' then
   {
      CP0.BadVAddr <- vAddr;
      SignalException (AdEL)
   }
   else
   {
      memdoubleword = LoadMemory (WORD, WORD, vAddr, DATA, LOAD, link);
      when not exceptionSignalled do
      {
         byte = vAddr<2:0> ?? (BigEndianCPU : '00');
         memword`32 = memdoubleword <31 + 8 * [byte] : 8 * [byte]>;
         GPR(rt) <- if unsigned then
                        ZeroExtend (memword)
                     else
                        SignExtend (memword)
      }
   }
}

unit loadDoubleword (link::bool, base::reg, rt::reg, offset::bits(16)) =
{
   vAddr = SignExtend (offset) + GPR(base);
   if vAddr<2:0> <> '000' then
   {
      CP0.BadVAddr <- vAddr;
      SignalException (AdEL)
   }
   else
   {
      memdoubleword = LoadMemory (DOUBLEWORD, DOUBLEWORD, vAddr, DATA, LOAD, link);
      when not exceptionSignalled do
         GPR(rt) <- memdoubleword
   }
}

--

define Load > LB (base::reg, rt::reg, offset::bits(16)) =
   loadByte (base, rt, offset, false)

define Load > LBU (base::reg, rt::reg, offset::bits(16)) =
   loadByte (base, rt, offset, true)

define Load > LH (base::reg, rt::reg, offset::bits(16)) =
   loadHalf (base, rt, offset, false)

define Load > LHU (base::reg, rt::reg, offset::bits(16)) =
   loadHalf (base, rt, offset, true)

define Load > LW (base::reg, rt::reg, offset::bits(16)) =
   loadWord (false, base, rt, offset, false)

define Load > LWU (base::reg, rt::reg, offset::bits(16)) =
   loadWord (false, base, rt, offset, true)

define Load > LL (base::reg, rt::reg, offset::bits(16)) =
   loadWord (true, base, rt, offset, false)

define Load > LD (base::reg, rt::reg, offset::bits(16)) =
   loadDoubleword (false, base, rt, offset)

define Load > LLD (base::reg, rt::reg, offset::bits(16)) =
   loadDoubleword (true, base, rt, offset)

-----------------------------------
-- LWL rt, offset(base)
-----------------------------------
define Load > LWL (base::reg, rt::reg, offset::bits(16)) =
{
   vAddr = SignExtend (offset) + GPR(base);
   byte = vAddr<1:0> ?? BigEndianCPU^2;
   word = vAddr<2:2> ?? BigEndianCPU;
   memdoubleword = LoadMemory (WORD, '0' : byte, vAddr, DATA, LOAD, false);
   when not exceptionSignalled do
   {
      temp`32 =
         match word, byte
         {
            case 0, 0 => memdoubleword <7:0>   : GPR(rt)<23:0>
            case 0, 1 => memdoubleword <15:0>  : GPR(rt)<15:0>
            case 0, 2 => memdoubleword <23:0>  : GPR(rt)<7:0>
            case 0, 3 => memdoubleword <31:0>
            case 1, 0 => memdoubleword <39:32> : GPR(rt)<23:0>
            case 1, 1 => memdoubleword <47:32> : GPR(rt)<15:0>
            case 1, 2 => memdoubleword <55:32> : GPR(rt)<7:0>
            case 1, 3 => memdoubleword <63:32>
         };
      GPR(rt) <- SignExtend (temp)
   }
}

-----------------------------------
-- LWR rt, offset(base)
-----------------------------------
define Load > LWR (base::reg, rt::reg, offset::bits(16)) =
{
   vAddr = SignExtend (offset) + GPR(base);
   byte = vAddr<1:0> ?? BigEndianCPU^2;
   word = vAddr<2:2> ?? BigEndianCPU;
   memdoubleword = LoadMemory (WORD, WORD - ('0' : byte), vAddr, DATA, LOAD, false);
   when not exceptionSignalled do
   {
      temp`32 =
         match word, byte
         {
            case 0, 0 =>                  memdoubleword <31:0>
            case 0, 1 => GPR(rt)<31:24> : memdoubleword <31:8>
            case 0, 2 => GPR(rt)<31:16> : memdoubleword <31:16>
            case 0, 3 => GPR(rt)<31:8>  : memdoubleword <31:24>
            case 1, 0 =>                  memdoubleword <63:32>
            case 1, 1 => GPR(rt)<31:24> : memdoubleword <63:40>
            case 1, 2 => GPR(rt)<31:16> : memdoubleword <63:48>
            case 1, 3 => GPR(rt)<31:8>  : memdoubleword <63:56>
         };
      GPR(rt) <- SignExtend (temp)
      -- alternative specification when byte specification <> 0 is
      -- GPR(rt)<31:0> <- temp
   }
}

-----------------------------------
-- LDL rt, offset(base)
-----------------------------------
define Load > LDL (base::reg, rt::reg, offset::bits(16)) =
{
   vAddr = SignExtend (offset) + GPR(base);
   byte = vAddr<2:0> ?? BigEndianCPU^3;
   memdoubleword = LoadMemory (DOUBLEWORD, byte, vAddr, DATA, LOAD, false);
   when not exceptionSignalled do
      GPR(rt) <-
         match byte
         {
            case 0 => memdoubleword <7:0>  : GPR(rt)<55:0>
            case 1 => memdoubleword <15:0> : GPR(rt)<47:0>
            case 2 => memdoubleword <23:0> : GPR(rt)<39:0>
            case 3 => memdoubleword <31:0> : GPR(rt)<31:0>
            case 4 => memdoubleword <39:0> : GPR(rt)<23:0>
            case 5 => memdoubleword <47:0> : GPR(rt)<15:0>
            case 6 => memdoubleword <55:0> : GPR(rt)<7:0>
            case 7 => memdoubleword <63:0>
         }
}

-----------------------------------
-- LDR rt, offset(base)
-----------------------------------
define Load > LDR (base::reg, rt::reg, offset::bits(16)) =
{
   vAddr = SignExtend (offset) + GPR(base);
   byte = vAddr<2:0> ?? BigEndianCPU^3;
   memdoubleword = LoadMemory (DOUBLEWORD, DOUBLEWORD - byte, vAddr, DATA, LOAD, false);
   when not exceptionSignalled do
      GPR(rt) <-
         match byte
         {
            case 0 =>                  memdoubleword <63:0>
            case 1 => GPR(rt)<63:56> : memdoubleword <63:8>
            case 2 => GPR(rt)<63:48> : memdoubleword <63:16>
            case 3 => GPR(rt)<63:40> : memdoubleword <63:24>
            case 4 => GPR(rt)<63:32> : memdoubleword <63:32>
            case 5 => GPR(rt)<63:24> : memdoubleword <63:40>
            case 6 => GPR(rt)<63:16> : memdoubleword <63:48>
            case 7 => GPR(rt)<63:8>  : memdoubleword <63:56>
         }
}

-----------------------------------
-- SB rt, offset(base)
-----------------------------------
define Store > SB (base::reg, rt::reg, offset::bits(16)) =
{
   vAddr = SignExtend (offset) + GPR(base);
   bytesel = vAddr<2:0> ?? BigEndianCPU^3;
   datadoubleword = GPR(rt) << (0n8 * [bytesel]);
   _ = StoreMemory (BYTE, BYTE, datadoubleword, vAddr, DATA, STORE, false);
   when not exceptionSignalled do LLbit <- None
}

-----------------------------------
-- SH rt, offset(base)
-----------------------------------
define Store > SH (base::reg, rt::reg, offset::bits(16)) =
{
   vAddr = SignExtend (offset) + GPR(base);
   if vAddr<0> then
   {
      CP0.BadVAddr <- vAddr;
      SignalException (AdES)
   }
   else
   {
      bytesel = vAddr<2:0> ?? (BigEndianCPU^2 : '0');
      datadoubleword = GPR(rt) << (0n8 * [bytesel]);
      _ = StoreMemory (HALFWORD, HALFWORD, datadoubleword, vAddr, DATA, STORE, false);
      when not exceptionSignalled do LLbit <- None
   }
}

-----------------------------------
-- SW  rt, offset(base)
-- SC  rt, offset(base)
-- SD  rt, offset(base)
-- SCD rt, offset(base)
-----------------------------------
bool storeWord (base::reg, rt::reg, offset::bits(16), cond::bool) =
{
   vAddr = SignExtend (offset) + GPR(base);
   var sc_success = false;
   if vAddr<1:0> <> '00' then
   {
      CP0.BadVAddr <- vAddr;
      SignalException (AdES)
   }
   else
   {
      bytesel = vAddr<2:0> ?? (BigEndianCPU : '00');
      datadoubleword = GPR(rt) << (0n8 * [bytesel]);
      sc_success <- StoreMemory (WORD, WORD, datadoubleword, vAddr, DATA, STORE, cond)
   };
   sc_success
}

bool storeDoubleword (base::reg, rt::reg, offset::bits(16), cond::bool) =
{
   vAddr = SignExtend (offset) + GPR(base);
   var sc_success = false;
   if vAddr<2:0> <> '000' then
   {
      CP0.BadVAddr <- vAddr;
      SignalException (AdES)
   }
   else
   {
      datadoubleword = GPR(rt);
      sc_success <- StoreMemory (DOUBLEWORD, DOUBLEWORD, datadoubleword, vAddr, DATA, STORE, cond)
   };
   sc_success
}

--

define Store > SW (base::reg, rt::reg, offset::bits(16)) =
{
   _ = storeWord (base, rt, offset, false);
   nothing
}

define Store > SD (base::reg, rt::reg, offset::bits(16)) =
{
   _ = storeDoubleword (base, rt, offset, false);
   nothing
}

define Store > SC (base::reg, rt::reg, offset::bits(16)) =
   GPR(rt) <- if storeWord (base, rt, offset, true) then 1 else 0

define Store > SCD (base::reg, rt::reg, offset::bits(16)) =
   GPR(rt) <- if storeDoubleword (base, rt, offset, true) then 1 else 0

-----------------------------------
-- SWL rt, offset(base)
-----------------------------------
define Store > SWL (base::reg, rt::reg, offset::bits(16)) =
{
   vAddr = SignExtend (offset) + GPR(base);
   byte = vAddr<1:0> ?? BigEndianCPU^2;
   word = vAddr<2:2> ?? BigEndianCPU;
   datadoubleword`64 =
      match byte
      {
        case 0 => [GPR(rt)<31:24>]
        case 1 => [GPR(rt)<31:16>]
        case 2 => [GPR(rt)<31:8>]
        case 3 => [GPR(rt)<31:0>]
      };
   datadoubleword =
      if word == '1' then datadoubleword << 32 else datadoubleword;
   vAddr = if BigEndianMem then vAddr else vAddr && ~0b11;
   _ = StoreMemory (WORD, [byte], datadoubleword, vAddr, DATA, STORE, false);
   nothing
}

-----------------------------------
-- SWR rt, offset(base)
-----------------------------------
define Store > SWR (base::reg, rt::reg, offset::bits(16)) =
{
   vAddr = SignExtend (offset) + GPR(base);
   byte = vAddr<1:0> ?? BigEndianCPU^2;
   word = vAddr<2:2> ?? BigEndianCPU;
   datadoubleword =
      match word, byte
      {
         case 0, 0 => [GPR(rt)<31:0>]
         case 0, 1 => [GPR(rt)<23:0>] << 8
         case 0, 2 => [GPR(rt)<15:0>] << 16
         case 0, 3 => [GPR(rt)<7:0>]  << 24
         case 1, 0 => [GPR(rt)<31:0>] << 32
         case 1, 1 => [GPR(rt)<23:0>] << 40
         case 1, 2 => [GPR(rt)<15:0>] << 48
         case 1, 3 => [GPR(rt)<7:0>]  << 56
      };
   vAddr = if BigEndianMem then vAddr && ~0b11 else vAddr;
   _ = StoreMemory (WORD, WORD - [byte], datadoubleword, vAddr, DATA, STORE, false);
   nothing
}

-----------------------------------
-- SDL rt, offset(base)
-----------------------------------
define Store > SDL (base::reg, rt::reg, offset::bits(16)) =
{
   vAddr = SignExtend (offset) + GPR(base);
   byte = vAddr<2:0> ?? BigEndianCPU^3;
   datadoubleword =
      match byte
      {
         case 0 => [GPR(rt)<63:56>]
         case 1 => [GPR(rt)<63:48>]
         case 2 => [GPR(rt)<63:40>]
         case 3 => [GPR(rt)<63:32>]
         case 4 => [GPR(rt)<63:24>]
         case 5 => [GPR(rt)<63:16>]
         case 6 => [GPR(rt)<63:8>]
         case 7 =>  GPR(rt)
      };
   vAddr = if BigEndianMem then vAddr else vAddr && ~0b111;
   _ = StoreMemory (DOUBLEWORD, byte, datadoubleword, vAddr, DATA, STORE, false);
   nothing
}

-----------------------------------
-- SDR rt, offset(base)
-----------------------------------
define Store > SDR (base::reg, rt::reg, offset::bits(16)) =
{
   vAddr = SignExtend (offset) + GPR(base);
   byte = vAddr<2:0> ?? BigEndianCPU^3;
   datadoubleword =
      match byte
      {
         case 0 =>  GPR(rt)
         case 1 => [GPR(rt)<55:0>] << 8
         case 2 => [GPR(rt)<47:0>] << 16
         case 3 => [GPR(rt)<39:0>] << 24
         case 4 => [GPR(rt)<31:0>] << 32
         case 5 => [GPR(rt)<23:0>] << 40
         case 6 => [GPR(rt)<15:0>] << 48
         case 7 => [GPR(rt)<7:0>] << 56
      };
   vAddr = if BigEndianMem then vAddr && ~0b111 else vAddr;
   _ = StoreMemory (DOUBLEWORD, DOUBLEWORD - byte, datadoubleword, vAddr, DATA, STORE, false);
   nothing
}

-----------------------------------
-- SYNC stype
-----------------------------------
define SYNC (stype::bits(5)) = nothing

-----------------------------------
-- BREAK
-----------------------------------
define BREAK = SignalException (Bp)

-----------------------------------
-- SYSCALL
-----------------------------------
define SYSCALL = SignalException (Sys)

-----------------------------------
-- MTC0 rt, rd
-- MTC0 rt, rd, sel
-----------------------------------
define CP > MTC0 (rt::reg, rd::reg, sel::bits(3)) =
   -- Will need adapting for EntryLo1 and EntryLo0
   if CP0.Status.CU0 or KernelMode then
      CPR (0, rd, sel) <- GPR(rt)
   else
      SignalException (CpU)

-----------------------------------
-- DMTC0 rt, rd
-- DMTC0 rt, rd, sel
-----------------------------------
define CP > DMTC0 (rt::reg, rd::reg, sel::bits(3)) =
   if CP0.Status.CU0 or KernelMode then
      CPR (0, rd, sel) <- GPR(rt)
   else
      SignalException (CpU)

-----------------------------------
-- MFC0 rt, rd
-- MFC0 rt, rd, sel
-----------------------------------
define CP > MFC0 (rt::reg, rd::reg, sel::bits(3)) =
   -- Will need adapting for EntryLo1 and EntryLo0; see manual entry for MFC0
   if CP0.Status.CU0 or KernelMode then
      GPR(rt) <- SignExtend (CPR (0, rd, sel)<31:0>)
   else
      SignalException (CpU)

-----------------------------------
-- DMFC0 rt, rd
-- DMFC0 rt, rd, sel
-----------------------------------
define CP > DMFC0 (rt::reg, rd::reg, sel::bits(3)) =
   if CP0.Status.CU0 or KernelMode then
      GPR(rt) <- CPR (0, rd, sel)
   else
      SignalException (CpU)

-----------------------------------
-- J target
-----------------------------------
define Branch > J (instr_index::bits(26)) =
   BranchTo <- Some ((PC<63:28> : instr_index : '00'))

-----------------------------------
-- JAL target
-----------------------------------
define Branch > JAL (instr_index::bits(26)) =
{
   GPR(31) <- PC + 8;
   BranchTo <- Some ((PC<63:28> : instr_index : '00'))
}

-----------------------------------
-- JR rs
-----------------------------------
define Branch > JR (rs::reg) =
   BranchTo <- Some (GPR(rs))

-----------------------------------
-- BEQ rs, rt, offset
-----------------------------------
define Branch > BEQ (rs::reg, rt::reg, offset::bits(16)) =
   if GPR(rs) == GPR(rt) then
      BranchTo <- Some (PC + 4 + SignExtend (offset) << 2)
   else
      CheckBranch

-----------------------------------
-- BNE rs, rt, offset
-----------------------------------
define Branch > BNE (rs::reg, rt::reg, offset::bits(16)) =
   if GPR(rs) <> GPR(rt) then
      BranchTo <- Some (PC + 4 + SignExtend (offset) << 2)
   else
      CheckBranch

-----------------------------------
-- BLEZ rs, offset
-----------------------------------
define Branch > BLEZ (rs::reg, offset::bits(16)) =
   if GPR(rs) <= 0 then
      BranchTo <- Some (PC + 4 + SignExtend (offset) << 2)
   else
      CheckBranch

-----------------------------------
-- BGTZ rs, offset
-----------------------------------
define Branch > BGTZ (rs::reg, offset::bits(16)) =
   if GPR(rs) > 0 then
      BranchTo <- Some (PC + 4 + SignExtend (offset) << 2)
   else
      CheckBranch

-----------------------------------
-- BLTZ rs, offset
-----------------------------------
define Branch > BLTZ (rs::reg, offset::bits(16)) =
   if GPR(rs) < 0 then
      BranchTo <- Some (PC + 4 + SignExtend (offset) << 2)
   else
      CheckBranch

-----------------------------------
-- BGEZ rs, offset
-----------------------------------
define Branch > BGEZ (rs::reg, offset::bits(16)) =
   if GPR(rs) >= 0 then
      BranchTo <- Some (PC + 4 + SignExtend (offset) << 2)
   else
      CheckBranch

-----------------------------------
-- BLTZAL rs, offset
-----------------------------------
define Branch > BLTZAL (rs::reg, offset::bits(16)) =
{
   temp = GPR(rs);
   GPR(31) <- PC + 8;
   if temp < 0 then
      BranchTo <- Some (PC + 4 + SignExtend (offset) << 2)
   else
      CheckBranch
}

-----------------------------------
-- BGEZAL rs, offset
-----------------------------------
define Branch > BGEZAL (rs::reg, offset::bits(16)) =
{
   temp = GPR(rs);
   GPR(31) <- PC + 8;
   if temp >= 0 then
      BranchTo <- Some (PC + 4 + SignExtend (offset) << 2)
   else
      CheckBranch
}

-----------------------------------
-- BEQL rs, rt, offset
-----------------------------------
define Branch > BEQL (rs::reg, rt::reg, offset::bits(16)) =
   if GPR(rs) == GPR(rt) then
      BranchTo <- Some (PC + 4 + SignExtend (offset) << 2)
   else
   {
      CheckBranch;
      PC <- PC + 4
   }

-----------------------------------
-- BNEL rs, rt, offset
-----------------------------------
define Branch > BNEL (rs::reg, rt::reg, offset::bits(16)) =
   if GPR(rs) <> GPR(rt) then
      BranchTo <- Some (PC + 4 + SignExtend (offset) << 2)
   else
   {
      CheckBranch;
      PC <- PC + 4
   }

-----------------------------------
-- BLEZL rs, offset
-----------------------------------
define Branch > BLEZL (rs::reg, offset::bits(16)) =
   if GPR(rs) <= 0 then
      BranchTo <- Some (PC + 4 + SignExtend (offset) << 2)
   else
   {
      CheckBranch;
      PC <- PC + 4
   }

-----------------------------------
-- BGTZL rs, offset
-----------------------------------
define Branch > BGTZL (rs::reg, offset::bits(16)) =
   if GPR(rs) > 0 then
      BranchTo <- Some (PC + 4 + SignExtend (offset) << 2)
   else
   {
      CheckBranch;
      PC <- PC + 4
   }

-----------------------------------
-- BLTZL rs, offset
-----------------------------------
define Branch > BLTZL (rs::reg, offset::bits(16)) =
   if GPR(rs) < 0 then
      BranchTo <- Some (PC + 4 + SignExtend (offset) << 2)
   else
   {
      CheckBranch;
      PC <- PC + 4
   }

-----------------------------------
-- BGEZL rs, offset
-----------------------------------
define Branch > BGEZL (rs::reg, offset::bits(16)) =
   if GPR(rs) >= 0 then
      BranchTo <- Some (PC + 4 + SignExtend (offset) << 2)
   else
   {
      CheckBranch;
      PC <- PC + 4
   }

-----------------------------------
-- BLTZALL rs, offset
-----------------------------------
define Branch > BLTZALL (rs::reg, offset::bits(16)) =
{
   temp = GPR(rs);
   GPR(31) <- PC + 8;
   if temp < 0 then
      BranchTo <- Some (PC + 4 + SignExtend (offset) << 2)
   else
   {
      CheckBranch;
      PC <- PC + 4
   }
}

-----------------------------------
-- BGEZALL rs, offset
-----------------------------------
define Branch > BGEZALL (rs::reg, offset::bits(16)) =
{
   temp = GPR(rs);
   GPR(31) <- PC + 8;
   if temp >= 0 then
      BranchTo <- Some (PC + 4 + SignExtend (offset) << 2)
   else
   {
      CheckBranch;
      PC <- PC + 4
   }
}

-----------------------------------
-- WAIT (implemented as no-op)
-----------------------------------

define WAIT = ()

-----------------------------------
-- Rerserved instruction, i.e. unsuccessful decode.
-----------------------------------
define ReservedInstruction =
   SignalException (ResI)

define Unpredictable = #UNPREDICTABLE("Unpredictable instruction")

define Run

-------------------------------------------------------
-- Not implemented:
--
-- LWCz, SWCz, MTCz, MFCz, CTCz, CFCz, COPz, BCzT, BCzF
-- DMFCz, DMTCz, LDCz, SDCz
-- BCzTL, BCzFL
-- CACHE
-- Floating-point
-------------------------------------------------------
