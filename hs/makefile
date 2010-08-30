# vim: tabstop=4:softtabstop=4:noexpandtab:list
all: lexer.hs HappyParser.hs

lexer.hs : parser.x
	alex parser.x -o lexer.hs

HappyParser.hs: parser.y lexer.hs Tracker.hs TrackAcc.hs 
	happy parser.y -a -g -d -c -o HappyParser.hs -iparser.info

