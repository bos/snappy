ghc := ghc

all: qc snappy

qc: Properties.hs
	$(ghc) --make -o $@ $^

snappy: Snappy.hs
	$(ghc) -O --make -o $@ $^

clean:
	-rm -f qc snappy *.o *.hi
