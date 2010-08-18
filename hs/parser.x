{
module Lexer where
import Data.Ratio
import Control.Monad.State


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
  --$commands     { \s -> TCommand s }
  \*            { \s -> TCommand (State (\c-> (("step", 1), "feet"))) }
  \,             { \s -> newCommand "feet" "step" (1/2) }
  \'             { \s -> newCommand "feet" "touch" (1/2) }
  \"             { \s -> newCommand "feet" "shuffle" (1) }
  :            { \s -> TCommand (return ("",1)) }
  \.            { \s -> TCommand (return ("",1/2)) }
  \^            { \s -> TCommand (State command_up) }

 

{
type Command = (String, Rational)
type MCommand = State String Command

newCommand :: String -> String -> Rational -> Token
newCommand channel command length  = TCommand (State (\c -> ((command, length), channel)))


data Token = TChannel String  |
             TLabel String |
             TSlash           |
             TPipe            |
             TNewline|
             TSpaces      |
             TCommand MCommand
             --deriving (Show, Eq);

command_up :: String -> (Command, String)
command_up "feet" = (("kick", 1), "feet")
command_up "body" = (("move forward", 2), "body")

lexer = alexScanTokens
run :: String -> Token -> (Command, String)
run s (TCommand m) = runState m s
}

