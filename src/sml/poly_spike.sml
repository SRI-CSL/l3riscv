structure Oracle :> Oracle =
struct

open Foreign

local
    val libRef : library option ref = ref NONE
in
fun getLib () =
    case (! libRef) of
        NONE =>
        let val cur_file = PolyML.getUseFileName ()
            val cur_dir  = case cur_file of
                               NONE => "."
                             | SOME p => OS.Path.dir p
            val lib_file = OS.Path.joinDirFile {dir  = cur_dir,
                                                file = "tvspike.so"}
            val lib      = loadLibrary lib_file
        in libRef := SOME lib
         ; lib
        end
     |  SOME lib => lib
val getSym = getSymbol (getLib ())
end

fun toBool i   = if i = 0 then false else true
fun fromBool b = if b     then 1     else 0

val init : unit -> unit =
    buildCall0 (getSym "tv_init", (), cVoid)

val loadElf : string -> unit =
    buildCall1 (getSym "tv_load_elf", cString, cVoid)

val setVerbose : bool -> unit =
    buildCall1 (getSym "tv_set_verbose", cInt, cVoid) o fromBool

val reset : unit -> unit =
    buildCall0 (getSym "tv_reset", (), cVoid)

val isDone : unit -> bool =
    toBool o buildCall0 (getSym "tv_is_done", (), cInt)

val checkPC : IntInf.int -> bool =
    toBool o buildCall1 (getSym "tv_check_pc", cUint64, cInt)
    o IntInf.toInt

val checkPriv : riscv.Privilege -> bool =
    toBool o buildCall1 (getSym "tv_check_priv", cUint8, cInt)
    o (fn p => case p of riscv.User       => 0
                      |  riscv.Supervisor => 1
                      |  riscv.Machine    => 3)

val checkGPR : (int * IntInf.int) -> bool =
    toBool o buildCall2 (getSym "tv_check_gpr", (cUint64, cUint64), cInt)
    o (fn (rno, rval) => (rno, IntInf.toInt rval))

val checkCSR : (int * IntInf.int) -> bool =
    toBool o buildCall2 (getSym "tv_check_csr", (cUint64, cUint64), cInt)
    o (fn (rno, rval) => (rno, IntInf.toInt rval))

end
