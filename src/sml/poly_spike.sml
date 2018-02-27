structure Oracle :> Oracle =
struct

fun init () = ()

fun loadElf file = ()

fun setVerbose enable = ()

fun isDone ()  = false

fun checkPC pc = true

fun checkPriv prv = true

fun checkGPR regno mval = true

fun checkCSR addr  mval = true

end
