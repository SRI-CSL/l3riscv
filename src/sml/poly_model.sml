(* Copyright (C) 2018  SRI International.
 *
 * This software was developed by SRI International and the University of
 * Cambridge Computer Laboratory under DARPA/AFRL contract FA8750-11-C-0249
 * ("MRC2"), as part of the DARPA MRC research programme, and under
 * DARPA/AFRL contract FA8750-10-C-0237 ("CTSRD"), as part of the DARPA
 * CRASH research programme, and under DARPA/AFRL contract FA8650-18-C-7809
 * ("CIFV"), and under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of
 * the DARPA SSITH research programme.
 *
 * See the LICENSE file for details.
 *)

(* Wrapper around Model.main() to catch runtime-specific FFI exceptions. *)

open Foreign

fun main () =
    model_main ()
    handle Foreign msg =>
           ( print ("FFI error: " ^ msg ^ "\n")
           ; print ("Either Spike support has not been built (see ENABLE_TVSPIKE in the Makefile), "
                    ^ "or is out of date.\n")
           )
        |  e           => print ("Exception error:" ^ (exnMessage e) ^ "\n")
