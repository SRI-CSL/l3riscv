---------------------------------------------------------------------------
-- Log utils
-- (c) Anthony Fox, University of Cambridge
---------------------------------------------------------------------------

--------------------------------------------------
-- Logging
--------------------------------------------------

string cpr (r::reg) =
   "c0_" :
   match r
   {
      case 0 => "index"
      case 1 => "random"
      case 2 => "entrylo0"
      case 3 => "entrylo1"
      case 4 => "context"
      case 5 => "pagemask"
      case 6 => "wired"
      case 7 => "hwrena"
      case 8 => "badvaddr"
      case 9 => "count"
      case 10 => "entryhi"
      case 11 => "compare"
      case 12 => "status"
      case 13 => "cause"
      case 14 => "epc"
      case 15 => "prid"
      case 16 => "config"
      case 17 => "lladdr"
      case 18 => "watchlo"
      case 19 => "watchhi"
      case 20 => "xcontext"
      case 21 => "21"
      case 22 => "22"
      case 23 => "debug"
      case 24 => "depc"
      case 25 => "perfcnt"
      case 26 => "errctl"
      case 27 => "cacheerr"
      case 28 => "taglo"
      case 29 => "taghi"
      case 30 => "errorepc"
      case 31 => "kscratch"
   }

string log_sig_exception (ExceptionCode::bits(5)) = "MIPS exception 0x" : PadLeft (#"0", 2, [ExceptionCode])
string log_w_gpr (r::reg, data::dword) = "Reg " : [[r]::nat] : " <- 0x" : PadLeft (#"0", 16, [data])
string log_w_hi (data::dword) = "HI <- 0x" : PadLeft (#"0", 16, [data])
string log_w_lo (data::dword) = "LO <- 0x" : PadLeft (#"0", 16, [data])
string log_w_c0 (r::reg, data::dword) = cpr(r) : " <- 0x" : PadLeft (#"0", 16, [data])

string log_w_mem (addr::bits(37), mask::bits(64), data::dword) =
    "MEM[0x" : PadLeft (#"0", 10, [addr]) :
    "] <- (data: 0x" : PadLeft (#"0", 16, [data]) :
    ", mask: 0x" : PadLeft (#"0", 16, [mask]) : ")"

string log_r_mem (addr::bits(37), data::dword) =
    "data <- MEM[0x" : PadLeft (#"0", 10, [addr]) :
    "]: 0x" : PadLeft (#"0", 16, [data])

declare log :: nat -> string list   -- One log per "trace level"

unit mark_log (lvl::nat, s::string) = log(lvl) <- s @ log(lvl)
unit unmark_log (lvl::nat) = log(lvl) <- Tail (log(lvl))
--unit clear_logs () = log <- InitMap(Nil)
unit clear_logs () = for i in 0 .. 5 do log(i) <- Nil

string hex32 (x::word)  = PadLeft (#"0", 8, [x])
string hex64 (x::dword) = PadLeft (#"0", 16, [x])
