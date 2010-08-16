{
module Lexer where
}

%wrapper "basic"
$channel = [\\\!\$\\@]
$alpha = [a-zA-Z]



tokens :-
  ^ [a-zA-Z][^:\n]* /:      { \s -> Tlabel s }
  "\#"$alpha+"#" { \s -> TChannel $(init.tail.tail) s }
  \\             { \s -> TChannel "feet" }
  \!             { \s -> TChannel "body" }
  \$             { \s -> TChannel "bearing" }
  \&             { \s -> TChannel "tangling" }

  "/"              { \s -> TSlash }
  "|"            {  \s -> TPipe }
  \n          { \s -> TNewline}
  "  \t"+   { \s -> TSpaces  }
  $printable     { \s -> TCommand s }

{

data Token = TChannel String  |
             Tlabel String |
             TSlash           |
             TPipe            |
             TNewline|
             TSpaces      |
             TCommand String
             deriving (Show, Eq);


}
