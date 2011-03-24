ghc := ghc
ghcflags := -threaded -O

all: qc snappy

qc: Properties.hs
	$(ghc) $(ghcflags) --make -o $@ $^

snappy: Snappy.hs
	$(ghc) $(ghcflags) -O --make -o $@ $^

clean:
	-rm -f qc snappy *.o *.hi
