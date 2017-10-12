(* Copyright (C) 2015-2017, SRI International.
 *
 * This software was developed by SRI International and the University
 * of Cambridge Computer Laboratory under DARPA/AFRL contract
 * FA8750-11-C-0249 ("MRC2"), as part of the DARPA MRC research
 * programme.
 *
 * See the LICENSE file for details.
 *)

signature Elf =
sig
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

    type elf_file

    val openElf : string -> elf_file

    val isELFFile           : elf_file -> bool
    val getElfHeader        : elf_file -> ehdr
    val getElfProgSegments  : elf_file -> ehdr -> pseg list
    val getElfProgSections  : elf_file -> ehdr -> psect list

    val printElfHeader      : ehdr  -> unit
    val printPSeg           : pseg  -> unit
    val printPSect          : psect -> unit
end
