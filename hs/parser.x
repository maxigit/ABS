{
module Lexer where
import Data.Ratio
import Control.Monad.State


}

%wrapper "basic"
$channel = [\\\!\$\\&]
$alpha = [a-zA-Z]
$spaces = [\ \t]
$actions = $printable # $spaces


-- we use action , as opposed to command, because an action has duration

tokens :-
  ^ [a-zA-Z][^:\n]* /:      { \s -> TLabel s }
  "\#"$alpha+"#" { \s -> TChannel $(init.tail.tail) s }
  $spaces*$channel$spaces* { \s -> stringToChannel s }
--  \\             { \s -> TChannel feet }
--  \!             { \s -> TChannel body }
--  \$             { \s -> TChannel "bearing" }
--  \&             { \s -> TChannel "tangling" }

  "/"              { \s -> TSlash }
  "|"            {  \s -> TPipe }
  \n          { \s -> TNewline}
  $spaces+   ;--{ \s -> TSpaces  }
  --$actions     { action $ s }
  \*            { action $ (State (\c-> (("step", 1), feet))) }
  \+            { action $ (State (\c-> (("hop", 1), feet))) }
  \,             { newAction feet "step" (1/2) }
  3             { newAction feet "step" (1/3) }
  \'             { newAction feet "touch" (1/2) }
  \"             { newAction feet "shuffle" (1) }
  :            { action $ (return ("rest",1)) }
  \.            { action $ (return ("rest",1/2)) }
  \^            { action $ (State action_up) }
  \>            { action $ (State action_right) }
  v            { action $ (State action_down) }
  \<            { action $ (State action_left) }
  \{             { newAction body "weight left" (2) }
  \{\{             { newAction body "shift left" (2) } -- whatever change the weight, whenever
  \}             { newAction body "weight right" (2) }
  \}\}             { newAction body "shift right" (2) }
  \%             { newAction body "rock step" (2) }
  \;              { newAction feet "step step " 2}
  _             { newAction body "even" (2) }
  o             { newAction feet "hold" 1 }
  \-             { newAction body "freeze" (2) }
  \@\@             { newAction body "2 spin" 2 }
  \@             { newAction body "spin" 2 }
  \?             { newAction body "1/2 turn" 2 }
  \?\?             { newAction body "1/4 turn" 2 }

  x     { newAction feet "cross step" 1 } -- pieds croises a cote
  z     { newAction feet "cross forward" 1 } -- 
  X     { newAction body "X stanse" 2 }
  A     { newAction body "A stanse" 2 } -- pied ecarte
  M     { newAction body "M stanse" 2 } -- pieds sous les epaules
  I     { newAction body "I stanse" 2 } -- pieds joints
  w     { newAction body "feet outward" 2 } -- mi
  m     { newAction body "feet inward" 2 } -- mi
  s     { newAction body "fold" 2 } -- strong compression, before or jump or stop
  S     { newAction body "bend" 2 }
  \~     { newAction body "slide" 0 }
  \~\~     { newAction body "Jump" 0 }

 

{
type Action = (String, Rational)
type MAction = State String Action

newAction :: String -> String -> Rational -> String -> Token
newAction channel action length s  = TAction (State (\c -> ((action, length), channel)))

action :: MAction -> a -> Token
action m s =  TAction m

feet = "Feet"
body = "Body"
orientation = "Orientation"
tangling = "Tangling"

data Token = TChannel String  |
             TLabel String |
             TSlash           |
             TPipe            |
             TNewline|
             TSpaces      |
             TAction MAction
             --deriving (Show, Eq);
stringToChannel :: String -> Token
stringToChannel (' ':s) = stringToChannel s
stringToChannel ('\t':s) = stringToChannel s
stringToChannel ('\\':s) = TChannel feet
stringToChannel ('!':s) = TChannel body
stringToChannel ('$':s) = TChannel orientation
stringToChannel ('&':s) = TChannel tangling


action_up :: String -> (Action, String)
action_up "Feet" = (("kick", 1), "Feet")
action_up "Body" = (("move forward", 2), "Body")

action_down :: String -> (Action, String)
action_down "Feet" = (("kick back", 1), "Feet")
action_down "Body" = (("move backward", 2), "Body")

action_left :: String -> (Action, String)
action_left "Feet" = (("kick left", 1), "Feet")
action_left "Body" = (("move left", 2), "Body")

action_right :: String -> (Action, String)
action_right "Feet" = (("kick right", 1), "Feet")
action_right "Body" = (("move right", 2), "Body")

lexer = alexScanTokens
run :: String -> Token -> (Action, String)
run s (TAction m) = runState m s
run s _ = (("", 0), s)
}
-- vim: ft=lex
