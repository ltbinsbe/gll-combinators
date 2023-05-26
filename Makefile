cabal:
	cabal v1-install --force-reinstalls

interface:
	ghci src/GLL/Combinators/Interface.hs -isrc:dist/build -XTypeOperators -XFlexibleInstances

bininterface:
	ghci src/GLL/Combinators/BinaryInterface.hs -isrc:dist/build -XTypeOperators -XFlexibleInstances

runtests: cabal 
	echo "main" | ghci src/GLL/Combinators/Test/Interface.hs -isrc:dist/build -XTypeOperators -XFlexibleInstances -XScopedTypeVariables -XTypeSynonymInstances -package regex-applicative -package pretty -package time -package text

runmemtests: cabal 
	echo "main" | ghci src/GLL/Combinators/Test/MemInterface.hs -isrc:dist/build -XTypeOperators -XFlexibleInstances -XScopedTypeVariables -XTypeSynonymInstances

unittest:
	ghci tests/interface/UnitTests.hs -isrc:dist/build -XTypeOperators -XFlexibleInstances -XScopedTypeVariables -XTypeSynonymInstances

runbintests: cabal
	echo "main" | ghci src/GLL/Combinators/Test/BinaryInterface.hs -isrc:dist/build

runmembintests: cabal
	echo "main" | ghci src/GLL/Combinators/Test/MemBinInterface.hs -isrc:dist/build -XTypeOperators -XFlexibleInstances -XScopedTypeVariables -XTypeSynonymInstances

