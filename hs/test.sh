function parse {
echo "-----------"
echo "$1"
echo "$1" | ./testParse 2>/dev/null | expand -t 32
echo 
echo
}
echo "***"
parse '* \ *'
parse "\\* \\*"
parse " % \\ % " 
parse "! % \\ % "
parse " *% \\ *% "

parse '*: * :
\ " ! v
" ! < >
*: *:
^
*: *:
! % { _ } 
" ! v'
