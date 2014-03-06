OCAMLMAKEFILE = OCamlMakefile

SOURCES = \
  extChar.mli \
  extChar.ml \
  extList.mli \
  extList.ml \
  extString.mli \
  extString.ml \
  extArray.mli \
  extArray.ml \
  fun.mli \
  fun.ml \
  io.mli \
  io.ml \
  fSet.mli \
  fSet.ml \
  fMap.mli \
  fMap.ml \
  dynArray.mli \
  dynArray.ml \
  priorityQueue.mli \
  priorityQueue.ml \
  charStream.mli \
  charStream.ml \
  mParser.mli \
  mParser.ml \
  caml.ml \
  std.ml

LIB_PACK_NAME = base

LIBINSTALL_FILES = \
  base.cmi base.cma base.cmxa base.a

PACKS = pcre

RESULT = base

OCAMLFLAGS = -for-pack Base

OCAMLNCFLAGS = -noassert -unsafe

OCAMLDOCFLAGS = -sort -colorize-code -t "The ocaml-base library"

DOC_DIR = ../doc

all: byte-code-library native-code-library 

docs: byte-code-library htdoc

install: libinstall

uninstall: libuninstall

include $(OCAMLMAKEFILE)
