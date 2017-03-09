retina: Lexer Parser OurMonad SemanticChecker
	ghc --make -w retina.hs

Lexer: Lexer.x
	alex Lexer.x

Parser: Parser.y
	happy Parser.y

OurMonad: OurMonad.hs
	ghc --make -w OurMonad.hs

SemanticChecker: SemanticChecker.hs
	ghc --make -w SemanticChecker.hs


AST: AST.hs
	ghc --make -w AST.hs

clean:
	-rm -f *.hi
	-rm -f *.o
	-rm -f Lexer.hs
	-rm -f Parser.hs
	-rm -f retina
