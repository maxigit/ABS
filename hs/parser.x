{
module Lexer where
}

%wrapper "basic"
$channel = [\\\!\$\\@]
$alpha = [a-zA-Z]
$spaces = [\ \t]
$commands = $printable # $spaces



tokens :-
  ^ [a-zA-Z][^:\n]* /:      { \s -> TLabel s }
  "\#"$alpha+"#" { \s -> TChannel $(init.tail.tail) s }
  \\             { \s -> TChannel "feet" }
  \!             { \s -> TChannel "body" }
  \$             { \s -> TChannel "bearing" }
  \&             { \s -> TChannel "tangling" }

  "/"              { \s -> TSlash }
  "|"            {  \s -> TPipe }
  \n          { \s -> TNewline}
  $spaces+   { \s -> TSpaces  }
  $commands     { \s -> TCommand s }

{

data Token = TChannel String  |
             TLabel String |
             TSlash           |
             TPipe            |
             TNewline|
             TSpaces      |
             TCommand String
             deriving (Show, Eq);


lexer = alexScanTokens
}

