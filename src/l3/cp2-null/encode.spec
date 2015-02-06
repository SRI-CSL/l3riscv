---------------------------------------------------------------------------
-- Place holders pretty printing for coprocessor 2
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

string COP2InstructionToString (i::instruction) = "default COP2 instruction"
string LWC2InstructionToString (i::instruction) = "default LWC2 instruction"
string LDC2InstructionToString (i::instruction) = "default LDC2 instruction"
string SWC2InstructionToString (i::instruction) = "default SWC2 instruction"
string SDC2InstructionToString (i::instruction) = "default SDC2 instruction"

word COP2Encode (i::instruction) = '010010' : 0
word LWC2Encode (i::instruction) = '110010' : 0
word LDC2Encode (i::instruction) = '110110' : 0
word SWC2Encode (i::instruction) = '111010' : 0
word SDC2Encode (i::instruction) = '111110' : 0
