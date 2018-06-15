#
# Makefile for CS 11 Haskell track, lab 4.
# Type 'make' to compile all files
# Type 'make clean' to delete the compiled programs and intermediate files
#

GHC    = ghc --make
HFLAGS = -W -O
PROGS  = hello1 hello2 hello3 hcat hsort huniq

compile: $(PROGS)

% : %.hs
	$(GHC) $(HFLAGS) $< $(LIBS) -o $@

check:
	haskell_style_check *.hs

clean:
	rm -f *.hi *.o $(PROGS)


