all: prog

prog: prog.hs
	ghc --make -i../src -L../combstruct/src -lcombstruct $<

clean:
	rm -f test prog *.o *.hi
