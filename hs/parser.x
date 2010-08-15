{
module Lexer where
}

%wrapper "basic"
$channel = [\\\!\$\\@]


tokens :-
  $channel    { \s -> TChannel s }
  \n          { \s -> TBegin }
  "  " | \t   { \s -> TIndent 1 }
  . # $channel           { \s -> TCommand s }

{

data Token = TChannel String  |
             TBegin           |
             TIndent Int      |
             TCommand String
             deriving (Show, Eq);
}
