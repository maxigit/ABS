# vim: tabstop=4:softtabstop=4:noexpandtab:list
all: lexer.hs parser.hs

lexer.hs : parser.x
	alex parser.x -o lexer.hs

parser.hs: parser.y lexer.hs Tracker.hs TrackAcc.hs 
	happy parser.y -o parser.hs

