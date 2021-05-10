MODULES = -iAbsLatte src/Parsing/AbsLatte.hs \
		-iLexLatte src/Parsing/LexLatte.hs \
		-iParLatte src/Parsing/ParLatte.hs \
		-iPrintLatte src/Parsing/PrintLatte.hs \
		-iSkelLatte src/Parsing/SkelLatte.hs

.PHONY : all clean

all : build/TestLatte build/Interpreter

%.hs : %.y
	happy --ghc --coerce --array --info $<

%.hs : %.x
	alex --ghc $<

build/Interpreter : src/Interpreter.hs src/Parsing/ErrM.hs \
		src/Parsing/LexLatte.hs src/Parsing/ParLatte.hs \
		src/Parsing/PrintLatte.hs
	ghc -dynamic --make $< -o $@ $(MODULES)

build/TestLatte : src/Parsing/TestLatte.hs src/Parsing/ErrM.hs \
		src/Parsing/LexLatte.hs src/Parsing/ParLatte.hs \
 		src/Parsing/PrintLatte.hs
	ghc -dynamic --make $< -o $@ $(MODULES)

clean :
	-rm -f build/*
	-cd src/Parsing && rm -f *.hi *.o *.log *.aux *.dvi