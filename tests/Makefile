pkgs := snappy

ghc := ghc
ghcflags := -Wall -threaded -O

all: bm qc speedy

bm: Functions.hs Benchmarks.hs
	$(ghc) $(ghcflags) --make -o $@ $^

qc: Functions.hs Properties.hs
	$(ghc) $(ghcflags) --make -o $@ $^

speedy: Speedy.hs
	$(ghc) $(ghcflags) --make -o $@ $^

clean:
	-rm -f bm qc speedy *.o *.hi
