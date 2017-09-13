all: FORCE
	rm -rf dist
	make allS

allS: FORCE
	(cabal install || make allS)

allR: FORCE
	rm -rf dist
	make allRS

allRS: FORCE
	(cabal install --force-reinstalls || make allRS)

profiler: FORCE
	rm -rf dist
	make profilerS

profilerS: FORCE
	(cabal install --enable-library-profiling --ghc-option=-auto-all || make profilerS)

profilerR: FORCE
	rm -rf dist
	make profilerRS

profilerRS: FORCE
	(cabal install --enable-library-profiling --force-reinstalls --ghc-option=-auto-all || make profilerRS)

doc: FORCE
	cabal haddock --hyperlink-source

reset: FORCE
	rm -rf dist
	rm -rf cabal.sandbox.config
	rm -rf .cabal-sandbox
	cabal sandbox init

FORCE: ;