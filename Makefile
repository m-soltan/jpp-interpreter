.PHONY : all clean

all : interpreter

%.hs : %.y
	happy --ghc --coerce --array --info $<

%.hs : %.x
	alex --ghc $<

interpreter : Src/Interpreter.hs Src/*.hs
	ghc -dynamic --make $< -o $@

build/TestLatte : Src/Parsing/TestLatte.hs Src/Parsing/ErrM.hs \
		Src/Parsing/LexLatte.hs Src/Parsing/ParLatte.hs \
		Src/Parsing/PrintLatte.hs
	ghc -dynamic --make $< -o $@


TMP_EXTENSIONS = *.aux *.dvi *.hi *.log *.o
clean :
	-rm -f interpreter
	-rm -f build/*
	-cd Src && rm -f $(TMP_EXTENSIONS)
	-cd Src/Parsing && rm -f $(TMP_EXTENSIONS)
