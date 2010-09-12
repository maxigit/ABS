function parse {
echo "-----------"
echo "$1"
echo "$1" | ./testParse 2>/dev/null | expand -t 20
}
echo "***"
parse '* \ *'
parse "\\* \\*"
parse " % \\ % " 
parse "! % \\ % "
parse " *% \\ *% "

parse '*: * :
*: *: !  < >
! % { _ }
'
