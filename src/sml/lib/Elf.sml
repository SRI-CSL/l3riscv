(* Copyright (C) 2015-2017, SRI International.
 *
 * This software was developed by SRI International and the University
 * of Cambridge Computer Laboratory under DARPA/AFRL contract
 * FA8750-11-C-0249 ("MRC2"), as part of the DARPA MRC research
 * programme.
 *
 * See the LICENSE file for details.
 *)

structure Elf : Elf = struct

datatype ElfType  = ET_NONE | ET_REL | ET_EXEC | ET_DYN | ET_CORE | ET_OTHER
datatype Endian   = BIG | LITTLE
datatype Class    = BIT_32 | BIT_64
datatype PType    = PT_NULL | PT_LOAD | PT_DYNAMIC | PT_INTERP | PT_NOTE | PT_SHLIB | PT_PHDR | PT_OTHER
datatype SType    = SHT_NULL | SHT_PROGBITS | SHT_SYMTAB | SHT_STRTAB | SHT_RELA | SHT_HASH | SHT_DYNAMIC | SHT_NOTE | SHT_NOBITS | SHT_REL | SHT_SHLIB | SHT_DYNSYM | SHT_OTHER

type ehdr =
     { etype:  ElfType
     , endian: Endian
     , class:  Class
     , entry:  LargeInt.int
     , phoff:  LargeInt.int
     , phesz:  LargeInt.int
     , phnum:  LargeInt.int
     , shoff:  LargeInt.int
     , shesz:  LargeInt.int
     , shnum:  LargeInt.int
     }

type pseg =
     { ptype:  PType
     , offset: LargeInt.int
     , paddr:  LargeInt.int
     , vaddr:  LargeInt.int
     , memsz:  LargeInt.int
     , bytes:  Word8Vector.vector
     }

type psect =
     { stype:  SType
     , snmidx: LargeInt.int
     , saddr:  LargeInt.int
     , soffs:  LargeInt.int
     , ssize:  LargeInt.int
     , sentsz: LargeInt.int
     }

type elf_file = Posix.IO.file_desc

(* type conversion utilities *)

fun toType t =
    case t of
        0 => ET_NONE
      | 1 => ET_REL
      | 2 => ET_EXEC
      | 3 => ET_DYN
      | 4 => ET_CORE
      | _ => ET_OTHER

fun typeToString t =
    case t of
        ET_NONE  => "NONE"
      | ET_REL   => "REL"
      | ET_EXEC  => "EXEC"
      | ET_DYN   => "DYN"
      | ET_CORE  => "CORE"
      | ET_OTHER => "OTHER"

fun toClass t =
    case t of
        1 => BIT_32
      | 2 => BIT_64
      | _ => raise Fail ("Invalid ELF Class " ^ IntInf.toString t)

fun classToString t =
    case t of
        BIT_32 => "32-bit"
      | BIT_64 => "64-bit"

fun toEndian t =
    case t of
        1 => LITTLE
      | 2 => BIG
      | _ => raise Fail ("Invalid ELF endian " ^ IntInf.toString t)

fun endianToString t =
    case t of
        LITTLE => "little-endian"
      | BIG    => "big-endian"

fun toPType t =
    case t of
        0 => PT_NULL
      | 1 => PT_LOAD
      | 2 => PT_DYNAMIC
      | 3 => PT_INTERP
      | 4 => PT_NOTE
      | 5 => PT_SHLIB
      | 6 => PT_PHDR
      | _ => PT_OTHER

fun ptypeToString t =
    case t of
        PT_NULL    => "NULL"
      | PT_LOAD    => "LOAD"
      | PT_DYNAMIC => "DYNAMIC"
      | PT_INTERP  => "INTERP"
      | PT_NOTE    => "NOTE"
      | PT_SHLIB   => "SHLIB"
      | PT_PHDR    => "PHDR"
      | PT_OTHER   => "OTHER"

fun toSType t =
    case t of
        0  => SHT_NULL
      | 1  => SHT_PROGBITS
      | 2  => SHT_SYMTAB
      | 3  => SHT_STRTAB
      | 4  => SHT_RELA
      | 5  => SHT_HASH
      | 6  => SHT_DYNAMIC
      | 7  => SHT_NOTE
      | 8  => SHT_NOBITS
      | 9  => SHT_REL
      | 10 => SHT_SHLIB
      | 11 => SHT_DYNSYM
      | _  => SHT_OTHER

fun stypeToString t =
    case t of
        SHT_NULL     => "NULL"
      | SHT_PROGBITS => "PROGBITS"
      | SHT_SYMTAB   => "SYMTAB"
      | SHT_STRTAB   => "STRTAB"
      | SHT_RELA     => "RELA"
      | SHT_HASH     => "HASH"
      | SHT_DYNAMIC  => "DYNAMIC"
      | SHT_NOTE     => "NOTE"
      | SHT_NOBITS   => "NOBITS"
      | SHT_REL      => "REL"
      | SHT_SHLIB    => "SHLIB"
      | SHT_DYNSYM   => "DYNSYM"
      | SHT_OTHER    => "OTHER"


(* binary processing utilities *)

fun extract_bin fd ofs width =
    let val ofs = Posix.IO.lseek(fd, ofs, Posix.IO.SEEK_SET)
    in  Posix.IO.readVec(fd, width)
    end

fun toInt endian v =
    (case endian of
         BIG    => Vector.foldli
      |  LITTLE => Vector.foldri
    ) (fn (i, v, acc) =>
          IntInf.orb (IntInf.<< (acc, Word.fromInt 8), IntInf.fromInt (Word8.toInt v))
      ) (IntInf.fromInt 0) v

(* debugging *)

fun show_bin name v =
    (print ("showing " ^ name ^ "\n");
     Vector.appi (fn (i,e) =>
                     print("\t " ^ (Int.toString i) ^ ": " ^ (Word8.toString e) ^ "\n")
                 ) v)

(* api *)

fun openElf fname =
    Posix.FileSys.openf (fname, Posix.FileSys.O_RDONLY, Posix.FileSys.O.sync)

val ELF_MAGIC = Vector.fromList [ 0x7F, 0x45, 0x4c, 0x46 ]
fun isELFFile fd =
    let val magic = extract_bin fd (Int64.fromInt 0) 4
    in  Vector.foldli
            (fn (idx, v, acc) =>
                acc andalso (Word8.fromInt (Vector.sub (ELF_MAGIC, idx)) = v)
            ) true magic
    end

fun entry_loc c = if c = BIT_32 then (24,4) else (24,8)
fun phoff_loc c = if c = BIT_32 then (28,4) else (32,8)
fun shoff_loc c = if c = BIT_32 then (32,4) else (40,8)
fun phesz_loc c = if c = BIT_32 then (42,2) else (54,2)
fun phnum_loc c = if c = BIT_32 then (44,2) else (56,2)
fun shesz_loc c = if c = BIT_32 then (46,2) else (58,2)
fun shnum_loc c = if c = BIT_32 then (48,2) else (60,2)

fun getElfHeader fd =
    let val class  = toClass  (toInt LITTLE (extract_bin fd 0x04 1))
        val endian = toEndian (toInt LITTLE (extract_bin fd 0x05 1))
        val etype  = toType   (toInt LITTLE (extract_bin fd 0x10 2))

        fun ex_field f    = let val (off, wd) = f
                            in  extract_bin fd off wd
                            end
        fun int_field loc = toInt endian (ex_field (loc class))

        val entry  = int_field entry_loc
        val phoff  = int_field phoff_loc
        val phesz  = int_field phesz_loc
        val phnum  = int_field phnum_loc
        val shoff  = int_field shoff_loc
        val shesz  = int_field shesz_loc
        val shnum  = int_field shnum_loc
    in
        { etype  = etype
        , endian = endian
        , class  = class
        , entry  = IntInf.toLarge entry
        , phoff  = IntInf.toLarge phoff
        , phesz  = IntInf.toLarge phesz
        , phnum  = IntInf.toLarge phnum
        , shoff  = IntInf.toLarge shoff
        , shesz  = IntInf.toLarge shesz
        , shnum  = IntInf.toLarge shnum
        }
    end

fun printElfHeader (ehdr : ehdr) =
    ( print ("ELF header\n")
    ; print ("\tType:   " ^ (typeToString (#etype ehdr))    ^ "\n")
    ; print ("\tEndian: " ^ (endianToString (#endian ehdr)) ^ "\n")
    ; print ("\tClass:  " ^ (classToString (#class ehdr))   ^ "\n")
    ; print ("\tEntry:  " ^ (IntInf.fmt StringCvt.HEX (#entry ehdr)) ^ "\n")
    ; print ("\tphoff:  " ^ (IntInf.fmt StringCvt.HEX (#phoff ehdr)) ^ "\n")
    ; print ("\tphesz:  " ^ (IntInf.fmt StringCvt.HEX (#phesz ehdr)) ^ "\n")
    ; print ("\tphnum:  " ^ (IntInf.fmt StringCvt.HEX (#phnum ehdr)) ^ "\n")
    ; print ("\tshoff:  " ^ (IntInf.fmt StringCvt.HEX (#shoff ehdr)) ^ "\n")
    ; print ("\tshesz:  " ^ (IntInf.fmt StringCvt.HEX (#shesz ehdr)) ^ "\n")
    ; print ("\tshnum:  " ^ (IntInf.fmt StringCvt.HEX (#shnum ehdr)) ^ "\n")
    )

(* Elf32_Phdr and Elf64_Phdr have different layouts for alignment! *)
fun ptype_loc  c = ( 0,4)
fun flags_loc  c = if c = BIT_32 then (24,4) else ( 4,4)
fun offset_loc c = if c = BIT_32 then ( 4,4) else ( 8,8)
fun vaddr_loc  c = if c = BIT_32 then ( 8,4) else (16,8)
fun paddr_loc  c = if c = BIT_32 then (12,4) else (24,8)
fun filesz_loc c = if c = BIT_32 then (16,4) else (32,8)
fun memsz_loc  c = if c = BIT_32 then (20,4) else (40,8)
fun align_loc  c = if c = BIT_32 then (28,4) else (48,8)
fun getPhdr (fd : elf_file) (ehdr : ehdr) i =
    let val c           = #class ehdr
        val endian      = #endian ehdr
        fun ex_field f  = let val (off, wd) = f
                              val ent_skip  = LargeInt.* (i, (#phesz ehdr))
                              val ent_off   = LargeInt.+ ((#phoff ehdr), ent_skip)
                              val fld_off   = LargeInt.+ (ent_off, off)
                          in  extract_bin fd (Int64.fromLarge fld_off) wd
                          end
        fun int_field loc = toInt endian (ex_field (loc c))
        val p_flags     = int_field flags_loc
        val p_offset    = int_field offset_loc
        val p_filesz    = int_field filesz_loc
        val offset      = Int64.fromLarge (IntInf.toLarge p_offset)
        val nbytes      = IntInf.toInt p_filesz
    in { ptype  = toPType (int_field ptype_loc)
       , offset = p_offset
       , paddr  = int_field paddr_loc
       , vaddr  = int_field vaddr_loc
       , memsz  = int_field memsz_loc
       , bytes  = extract_bin fd offset nbytes
       }
    end

fun printPSeg (pseg : pseg) =
    ( print ("Segment:\n")
    ; print ("\tType:     " ^ (ptypeToString   (#ptype pseg)) ^ "\n")
    ; print ("\tOffset:   " ^ (IntInf.fmt StringCvt.HEX (#offset pseg)) ^ "\n")
    ; print ("\tVirtAddr: " ^ (IntInf.fmt StringCvt.HEX (#vaddr  pseg)) ^ "\n")
    ; print ("\tPhysAddr: " ^ (IntInf.fmt StringCvt.HEX (#paddr  pseg)) ^ "\n")
    ; print ("\tMemSize:  "
             ^ (IntInf.fmt StringCvt.HEX (#memsz  pseg))
             ^ " (" ^ (IntInf.toString (#memsz  pseg)) ^ ")"
             ^ "\n")
    ; print ("\tFileSize: "
             ^ (Int.fmt StringCvt.HEX (Word8Vector.length (#bytes pseg)))
             ^ " (" ^ (IntInf.toString (Word8Vector.length (#bytes pseg))) ^ ")"
             ^ "\n")
    )

fun getElfProgSegments fd (ehdr : ehdr) =
    let val nsegs    = IntInf.toInt (#phnum ehdr)
        val segnlist = List.tabulate (nsegs, (fn i => Int.toLarge i))
    in  List.map (getPhdr fd ehdr) segnlist
    end

(* Elf32_Shdr and Elf64_Shdr have different layouts. *)
fun snmidx_loc c = (0,4)
fun stype_loc  c = (4,4)
fun saddr_loc  c = if c = BIT_32 then (12,4) else (16,8)
fun soffs_loc  c = if c = BIT_32 then (16,4) else (24,8)
fun ssize_loc  c = if c = BIT_32 then (20,4) else (32,8)
fun sentsz_loc c = if c = BIT_32 then (36,4) else (56,8)
fun getShdr (fd : elf_file) (ehdr : ehdr) i =
    let val c           = #class ehdr
        val endian      = #endian ehdr
        fun ex_field f  = let val (off, wd) = f
                              val ent_skip  = LargeInt.* (i, (#shesz ehdr))
                              val ent_off   = LargeInt.+ ((#shoff ehdr), ent_skip)
                              val fld_off   = LargeInt.+ (ent_off, off)
                          in  extract_bin fd (Int64.fromLarge fld_off) wd
                          end
        fun int_field loc = toInt endian (ex_field (loc c))
    in { snmidx = int_field snmidx_loc
       , stype  = toSType (int_field stype_loc)
       , saddr  = int_field saddr_loc
       , soffs  = int_field soffs_loc
       , ssize  = int_field ssize_loc
       , sentsz = int_field sentsz_loc
       }
    end

fun printPSect (psect : psect) =
    ( print ("Section:\n")
    ; print ("\tType:    " ^ (stypeToString (#stype psect)) ^ "\n")
    ; print ("\tNameIdx: " ^ (IntInf.fmt StringCvt.HEX (#snmidx psect)) ^ "\n")
    ; print ("\tAddress: " ^ (IntInf.fmt StringCvt.HEX (#saddr  psect)) ^ "\n")
    ; print ("\tOffset:  " ^ (IntInf.fmt StringCvt.HEX (#soffs  psect)) ^ "\n")
    ; print ("\tSize:    " ^ (IntInf.fmt StringCvt.HEX (#ssize  psect)) ^ "\n")
    ; print ("\tEntSize: " ^ (IntInf.fmt StringCvt.HEX (#sentsz psect)) ^ "\n")
    )

fun getElfProgSections fd (ehdr : ehdr) =
    let val nsects    = IntInf.toInt (#shnum ehdr)
        val sectnlist = List.tabulate (nsects, (fn i => Int.toLarge i))
    in  List.map (getShdr fd ehdr) sectnlist
    end

end (* struct *)
