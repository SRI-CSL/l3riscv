(* Copyright (C) 2015, SRI International.
 * See the file LICENSE for details.
 *)

signature Elf =
sig
    datatype ElfType  = ET_NONE | ET_REL | ET_EXEC | ET_DYN | ET_CORE | ET_OTHER
    datatype Endian   = BIG | LITTLE
    datatype Class    = BIT_32 | BIT_64
    datatype PType    = PT_NULL | PT_LOAD | PT_DYNAMIC | PT_INTERP | PT_NOTE | PT_SHLIB | PT_PHDR | PT_OTHER

    type ehdr =
         { etype:  ElfType
         , endian: Endian
         , class:  Class
         , entry:  LargeInt.int
         , phoff:  LargeInt.int
         , phesz:  LargeInt.int
         , phnum:  LargeInt.int
         }

    type pseg =
         { ptype:  PType
         , offset: LargeInt.int
         , paddr:  LargeInt.int
         , vaddr:  LargeInt.int
         , memsz:  LargeInt.int
         , bytes:  Word8Vector.vector
         }

    type elf_file

    val openElf : string -> elf_file

    val isELFFile           : elf_file -> bool
    val getElfHeader        : elf_file -> ehdr
    val getElfProgSegments  : elf_file -> ehdr -> pseg list

    val printElfHeader      : ehdr -> unit
    val printPSeg           : pseg -> unit
end
