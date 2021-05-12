MODULES = -iAbsLatte Src/Parsing/AbsLatte.hs \
		-iLexLatte Src/Parsing/LexLatte.hs \
		-iParLatte Src/Parsing/ParLatte.hs \
		-iPrintLatte Src/Parsing/PrintLatte.hs \
		-iSkelLatte Src/Parsing/SkelLatte.hs \
		-iInterpreter Src/Interpreter.hs

.PHONY : all clean

all : interpreter

%.hs : %.y
	happy --ghc --coerce --array --info $<

%.hs : %.x
	alex --ghc $<

interpreter : Src/*.hs
	ghc -dynamic --make $< -o $@ $(MODULES)

build/TestLatte : Src/Parsing/TestLatte.hs Src/Parsing/ErrM.hs \
		Src/Parsing/LexLatte.hs Src/Parsing/ParLatte.hs \
		Src/Parsing/PrintLatte.hs
	ghc -dynamic --make $< -o $@ $(MODULES)


TMP_EXTENSIONS = *.aux *.dvi *.hi *.log *.o
clean :
	-rm -f interpreter
	-rm -f build/*
	-cd Src && rm -f $(TMP_EXTENSIONS)
	-cd Src/Parsing && rm -f $(TMP_EXTENSIONS)