
OCB_FLAGS = -use-ocamlfind \
			-I src/ml -I src/ \
			-pkg lwt,lwt.unix,unix \
			-lflags src/c/gopigo_stubs.o

OCB = 		ocamlbuild $(OCB_FLAGS)

clean:
			$(OCB) -clean

src/c/gopigo_stubs.o:src/c/gopigo_stubs.c
	ocamlbuild src/c/gopigo_stubs.o

main.native:src/c/gopigo_stubs.o
	$(OCB) main.native

speed_calibrator.native:src/c/gopigo_stubs.o
	$(OCB) speed_calibrator.native

position_calibrator.native:src/c/gopigo_stubs.o
	$(OCB) position_calibrator.native

all: main.native speed_calibrator.native position_calibrator.native 

.PHONY:all native clean
