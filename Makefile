## File generated by the BNF Converter (bnfc 2.9.4).

# Makefile for building the parser and test program.

GHC        = ghc
HAPPY      = happy
HAPPY_OPTS = --array --info --ghc --coerce
ALEX       = alex
ALEX_OPTS  = --ghc

# List of goals not corresponding to file names.

.PHONY : all clean distclean

# Default goal.

all : Latte/Test compiler

compiler: Latte/Abs.hs Latte/Lex.hs Latte/Par.hs Latte/Print.hs Latte/Test.hs src/*.hs
	ghc -isrc src/main.hs -o compiler

# Rules for building the parser.

Latte/Abs.hs Latte/Lex.x Latte/Par.y Latte/Print.hs Latte/Test.hs : Latte.cf
	bnfc --functor --haskell -d Latte.cf

%.hs : %.y
	${HAPPY} ${HAPPY_OPTS} $<

%.hs : %.x
	${ALEX} ${ALEX_OPTS} $<

Latte/Test : Latte/Abs.hs Latte/Lex.hs Latte/Par.hs Latte/Print.hs Latte/Test.hs
	${GHC} ${GHC_OPTS} $@

# Rules for cleaning generated files.

clean :
	-rm -f Latte/*.hi Latte/*.o Latte/*.log Latte/*.aux Latte/*.dvi`

# EOF
