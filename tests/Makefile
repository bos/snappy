ghc := ghc

all: qc

qc: Properties.hs
	$(ghc) --make -o $@ $^

clean:
	-rm -f qc *.o *.hi
