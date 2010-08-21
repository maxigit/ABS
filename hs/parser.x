{
module Lexer where
import Data.Ratio
import Control.Monad.State


}

%wrapper "basic"
$channel = [\\\!\$\\@]
$alpha = [a-zA-Z]
$spaces = [\ \t]
$actions = $printable # $spaces


-- we use action , as opposed to command, because an action has duration

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
  --$actions     { action $ s }
  \*            { action $ (State (\c-> (("step", 1), "feet"))) }
  \,             { newAction "feet" "step" (1/2) }
  \'             { newAction "feet" "touch" (1/2) }
  \"             { newAction "feet" "shuffle" (1) }
  :            { action $ (return ("rest",1)) }
  \.            { action $ (return ("rest",1/2)) }
  \^            { action $ (State action_up) }

 

{
type Action = (String, Rational)
type MAction = State String Action

newAction :: String -> String -> Rational -> String -> Token
newAction channel action length s  = TAction (State (\c -> ((action, length), channel)))

action :: MAction -> a -> Token
action m s =  TAction m


data Token = TChannel String  |
             TLabel String |
             TSlash           |
             TPipe            |
             TNewline|
             TSpaces      |
             TAction MAction
             --deriving (Show, Eq);

action_up :: String -> (Action, String)
action_up "feet" = (("kick", 1), "feet")
action_up "body" = (("move forward", 2), "body")

lexer = alexScanTokens
run :: String -> Token -> (Action, String)
run s (TAction m) = runState m s
run s _ = (("", 0), s)
}

