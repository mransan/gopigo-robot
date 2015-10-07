
OCB_FLAGS = -use-ocamlfind \
			-I src/ml -I src/ \
			-pkg lwt,lwt.unix,unix \
			-lflags src/c/gopigo_stubs.o

OCB = 		ocamlbuild $(OCB_FLAGS)

clean:
			$(OCB) -clean

src/c/gopigo_stubs.o:src/c/gopigo_stubs.c
	ocamlbuild src/c/gopigo_stubs.o

native:src/c/gopigo_stubs.o
	$(OCB) main.native

all: native 

.PHONY:all native clean
