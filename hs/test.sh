function parse {
echo "-----------"
echo "$1"
echo "$1" | ./testParse 2>/dev/null | expand -t 24
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
! % { _ } 
! % { _ } 
" ! v \ *'


parse " ! < > (<><>)"
parse " (,,,) (*) * "
