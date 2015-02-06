string r (n::reg) = "$" : [[n]::nat]
string c (n::reg) = ", " : r(n)
string i (n::bits(N)) = ", " : (if n <+ 10 then "" else "0x") : [n]
string oi (n::bits(N)) = if n == 0 then "" else i(n)

string op1i (s::string, n::bits(N)) = PadRight (#" ", 12, s) : "0x" : [n]
string op1r (s::string, n::reg) = PadRight (#" ", 12, s) : r(n)
string op1ri (s::string, r1::reg, n::bits(N)) = op1r(s,r1) : i(n)
string op2r (s::string, r1::reg, r2::reg) = op1r(s,r1) : c(r2)
string op2ri (s::string, r1::reg, r2::reg, n::bits(N)) = op2r(s,r1,r2) : i(n)
string op3r (s::string, r1::reg, r2::reg, r3::reg) = op2r(s,r1,r2) : c(r3)
string op3ro (s::string, r1::reg, r2::reg, r3::reg, n::bits(N)) =
   op2r(s,r1,r3) : i(n) : "(" : r(r2) : ")"
string op2roi (s::string, r1::reg, r2::reg, n::bits(N)) =
   op1r(s,r1) : ", " : cpr (r2) : oi(n)

string opmem (s::string, r1::reg, r2::reg, n::bits(N)) =
   op1ri(s,r1,n):"(":r(r2):")"

word form1 (rs::reg, rt::reg, rd::reg, imm5::bits(5), function::bits(6)) =
   '000000' : rs : rt : rd : imm5 : function

word form2 (rs::reg, function::bits(5), imm::bits(16)) =
   '000001' : rs : function : imm

word form3 (function::bits(5), rt::reg, rd::reg, sel::bits(3)) =
   '010000' : function : rt : rd : '00000000' : sel

word form4 (function::bits(6), rs::reg, rt::reg, imm::bits(16)) =
   function : rs : rt : imm

word form5 (rs::reg, rt::reg, rd::reg, function::bits(6)) =
   '011100' : rs : rt : rd : '00000' : function

word form6 (rt::reg, rd::reg, function::bits(6)) =
   '011111' : '00000' : rt : rd : '00000' : function
