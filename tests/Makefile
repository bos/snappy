ghc := ghc
ghcflags := -threaded -O

all: qc speedy

qc: Properties.hs
	$(ghc) $(ghcflags) --make -o $@ $^

speedy: Speedy.hs
	$(ghc) $(ghcflags) -O --make -o $@ $^

clean:
	-rm -f qc speedy *.o *.hi
