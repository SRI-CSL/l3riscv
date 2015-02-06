---------------------------------------------------------------------------
-- Place holders for coprocessor 2 instructions
-- (c) Alexandre Joannou, University of Cambridge
---------------------------------------------------------------------------

define COP2 (v::bits(26)) = if not CP0.Status.CU2 then SignalCP2UnusableException else nothing
define LWC2 (v::bits(26)) = if not CP0.Status.CU2 then SignalCP2UnusableException else nothing
define LDC2 (v::bits(26)) = if not CP0.Status.CU2 then SignalCP2UnusableException else nothing
define SWC2 (v::bits(26)) = if not CP0.Status.CU2 then SignalCP2UnusableException else nothing
define SDC2 (v::bits(26)) = if not CP0.Status.CU2 then SignalCP2UnusableException else nothing
