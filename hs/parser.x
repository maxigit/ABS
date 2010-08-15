{
module Lexer where
}

%wrapper "basic"
$channel = [\\\!\$\\@]
$alpha = [a-zA-Z]



tokens :-
  "\#"$alpha+"#" { \s -> TChannel $(init.tail.tail) s }

  \\             { \s -> TChannel "feet" }
  \!             { \s -> TChannel "body" }
  \$             { \s -> TChannel "bearing" }
  \&             { \s -> TChannel "tangling" }

  "/"              { \s -> TSlash }
  "|"            {  \s -> TPipe }
  \n          { \s -> TBegin }
  "  " | \t   { \s -> TIndent 1 }
  $printable     { \s -> TCommand s }

{

data Token = TChannel String  |
             TSlash           |
             TPipe            |
             TBegin           |
             TIndent Int      |
             TCommand String
             deriving (Show, Eq);


}
