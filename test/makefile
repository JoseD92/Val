GHC = ghc
EXTRAFLAGS = -ddump-simpl -ddump-to-file
THREADED = -threaded
THREADEDTEST = $(THREADED) -eventlog -rtsopts
GHCFLAGS = $(PACKAGE) --make -O2
PACKAGE = -package-db=../.cabal-sandbox/x86_64-windows-ghc-8.0.2-packages.conf.d
PROFILEFLAGS = -prof -fprof-auto

all: test.exe

test.exe: FORCE
	$(GHC) $(GHCFLAGS) test.hs

thread: FORCE
	$(GHC) $(GHCFLAGS) $(THREADED) test.hs

threadtest: FORCE
	$(GHC) $(GHCFLAGS) $(THREADEDTEST) test.hs

profiler: FORCE
	$(GHC) $(GHCFLAGS) test.hs $(PROFILEFLAGS)
	
runProf: FORCE
	./test.exe +RTS -p
	hp2ps test.hp
	rm -f test.pdf
	ps2pdf test.ps

runStack: FORCE
	./test.exe +RTS -hc
	hp2ps test.hp
	rm -f test.pdf
	ps2pdf test.ps

runClou: FORCE
	./test.exe +RTS -hd
	hp2ps test.hp
	rm -f test.pdf
	ps2pdf test.ps
	
FORCE: ;

clean: 
	rm -rf *.o *.hi *.exe *.pdf *.aux *.hp *.prof *.ps *.dump-simpl *.eventlog