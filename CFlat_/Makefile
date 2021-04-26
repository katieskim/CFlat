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
	rm -rf testall.log ocamlllvm 
	rm -f *.o *.out *.mid *.ll *.s *. *.exe *.diff *.err

# Testing the "printbig" example

printbig: printbig.c
	cc -o printbig -DBUILD_TEST printbig.c

# midifiles
midifile.o: midifile.c	midifile.h

cflatapi.o: cflatapi.c cflatapi.h

# Building the tarball

TESTS = microcfloat1 hellonote

# FAILS = microcprint

# TESTFILES = $(TESTS:%=test-%.cf) $(TESTS:%=test-%.out) \
#             $(FAILS:%=fail-%.cf) $(FAILS:%=fail-%.err)

TESTFILES = $(TESTS:%=test-%.cf) $(TESTS:%=test-%.out)

TARFILES = ast.ml sast.ml codegen.ml Makefile _tags cflat.ml parser.mly \
	README scanner.mll semant.ml testall.sh \
	printbig.c cflatapi.c midifile.c arcade-font.pbm font2c \
	Dockerfile \
	$(TESTFILES:%=tests/%) 

cflat.tar.gz : $(TARFILES)
	cd .. && tar czf cflat/cflat.tar.gz \
		$(TARFILES:%=cflat/%)
