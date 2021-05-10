modules = -iAbsLatte src/Parsing/AbsLatte.hs \
		-iLexLatte src/Parsing/LexLatte.hs \
		-iParLatte src/Parsing/ParLatte.hs \
		-iPrintLatte src/Parsing/PrintLatte.hs \
		-iSkelLatte src/Parsing/SkelLatte.hs

.PHONY : all clean distclean

all : src/Parsing/TestLatte

# Rules for building the parser.

%.hs : %.y
	happy --ghc --coerce --array --info $<

%.hs : %.x
	alex --ghc $<

src/Parsing/TestLatte : src/Parsing/*.hs
	ghc -dynamic --make $< -o $@ $(modules)
		

# Rules for cleaning generated files.

clean :
	-rm -f src/Parsing/*.hi src/Parsing/*.o src/Parsing/*.log src/Parsing/*.aux src/Parsing/*.dvi

distclean : clean
	-rm -f src/Parsing/AbsLatte.hs src/Parsing/AbsLatte.hs.bak src/Parsing/ComposOp.hs src/Parsing/ComposOp.hs.bak src/Parsing/DocLatte.txt src/Parsing/DocLatte.txt.bak src/Parsing/ErrM.hs src/Parsing/ErrM.hs.bak src/Parsing/LayoutLatte.hs src/Parsing/LayoutLatte.hs.bak src/Parsing/LexLatte.x src/Parsing/LexLatte.x.bak src/Parsing/ParLatte.y src/Parsing/ParLatte.y.bak src/Parsing/PrintLatte.hs src/Parsing/PrintLatte.hs.bak src/Parsing/SkelLatte.hs src/Parsing/SkelLatte.hs.bak src/Parsing/TestLatte.hs src/Parsing/TestLatte.hs.bak src/Parsing/XMLLatte.hs src/Parsing/XMLLatte.hs.bak src/Parsing/ASTLatte.agda src/Parsing/ASTLatte.agda.bak src/Parsing/ParserLatte.agda src/Parsing/ParserLatte.agda.bak src/Parsing/IOLib.agda src/Parsing/IOLib.agda.bak src/Parsing/Main.agda src/Parsing/Main.agda.bak src/Parsing/Latte.dtd src/Parsing/Latte.dtd.bak src/Parsing/TestLatte src/Parsing/LexLatte.hs src/Parsing/ParLatte.hs src/Parsing/ParLatte.info src/Parsing/ParDataLatte.hs Makefile
	-rmdir -p src/Parsing/

# EOF
