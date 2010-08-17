# vim: tabstop=4:softtabstop=4:noexpandtab
lexer.hs : parser.x
	alex parser.x -o lexer.hs

parser.hs: parser.y
	happy parser.y -o parser.hs

all: lexer.hs parser.hs
