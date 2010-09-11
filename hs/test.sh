function parse {
echo "-----------"
echo $1
echo $1 | ./testParse 2>/dev/null | expand -t 20
}
parse " % \\ % " 
parse "! % \\ % "
