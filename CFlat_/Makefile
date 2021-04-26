CC=gcc
LINK = $(CC)
CFLAGS = -O2 -ansi -Wall
LDFLAGS = -s

# "make test" Compiles everything and runs the regression tests

.PHONY : test
test : all testall.sh
	./testall.sh

# "make all" builds the executable as well as the "printbig" library designed
# to test linking external code

.PHONY : all
all : cflat.native printbig.o cflatapi.o midifile.o

# "make cflat.native" compiles the compiler
#
# The _tags file controls the operation of ocamlbuild, e.g., by including
# packages, enabling warnings
#
# See https://github.com/ocaml/ocamlbuild/blob/master/manual/manual.adoc

cflat.native :
	opam config exec -- \
	ocamlbuild -use-ocamlfind cflat.native

# "make clean" removes all generated files

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf testall.log ocamlllvm *.diff
	rm -f *.o  *.mid

# Testing the "printbig" example

printbig: printbig.c
	cc -o printbig -DBUILD_TEST printbig.c

# midifiles
midifile.o: midifile.c	midifile.h

cflatapi.o: cflatapi.c cflatapi.h

# Building the tarball
TESTS = arr_assign_int arr_assign_note arr_concat_int arr_concat_int arr_decl_int arr_make_int arr_make_note\
		binop0 binop1 bplaynote for0 for1 helloworld0 helloworld1 if0 if1 if2 int microcfloat1 note noteatt1_declare\
		noteatt2_get noteatt3_getset playnote printfloat tone_features unop0 while0

FAILS = assign_int_note
# FAILS = microcprint

MIDI = playnote bplaynote 

TESTFILES = $(TESTS:%=test-%.cf) $(TESTS:%=test-%.out) \
            $(FAILS:%=fail-%.cf) $(FAILS:%=fail-%.err) \
			$(MIDI:%=midi-%.cf) $(MIDI:%=midi-%.mid)

TARFILES = ast.ml sast.ml codegen.ml Makefile _tags cflat.ml parser.mly \
	README scanner.mll semant.ml testall.sh \
	printbig.c cflatapi.c midifile.c arcade-font.pbm font2c \
	Dockerfile \
	$(TESTFILES:%=tests/%) 

cflat.tar.gz : $(TARFILES)
	cd .. && tar czf cflat/cflat.tar.gz \
		$(TARFILES:%=cflat/%)
