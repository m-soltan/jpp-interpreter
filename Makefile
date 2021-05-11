MODULES = -iAbsLatte src/Parsing/AbsLatte.hs \
		-iLexLatte src/Parsing/LexLatte.hs \
		-iParLatte src/Parsing/ParLatte.hs \
		-iPrintLatte src/Parsing/PrintLatte.hs \
		-iSkelLatte src/Parsing/SkelLatte.hs \
		\
		-iMemory src/Memory.hs \
		-iProgram src/Program.hs \
		-iTopDef src/TopDef.hs \
		-iTopScope src/TopScope.hs \
		-iUtil src/Util.hs

.PHONY : all clean

all : build/TestLatte interpreter

%.hs : %.y
	happy --ghc --coerce --array --info $<

%.hs : %.x
	alex --ghc $<

interpreter : src/*
	ghc -dynamic --make $< -o $@ $(MODULES)

build/TestLatte : src/Parsing/TestLatte.hs src/Parsing/ErrM.hs \
		src/Parsing/LexLatte.hs src/Parsing/ParLatte.hs \
 		src/Parsing/PrintLatte.hs
	ghc -dynamic --make $< -o $@ $(MODULES)


TMP_EXTENSIONS = *.aux *.dvi *.hi *.log *.o
clean :
	-rm -f interpreter
	-rm -f build/*
	-cd src && rm -f $(TMP_EXTENSIONS)
	-cd src/Parsing && rm -f $(TMP_EXTENSIONS)