{
module Lexer where
import Data.Ratio
import Control.Monad.State
import qualified Data.Map as M
import qualified SpinLexer as S
}

%wrapper "basic"
$channel = [\\\!\$\\&]
$alpha = [a-zA-Z]
$spaces = [\ \t]
$actions = $printable # $spaces


-- we use action , as opposed to command, because an action has duration

tokens :-
  --^[a-zA-Z][^:\n]* /:      { \s -> TLabel s }
  "\#"$alpha+"#" { \s -> TChannel $(init.tail.tail) s }
  $spaces*$channel$spaces+ { \s -> stringToChannel s }
--  \\             { \s -> TChannel feet }
--  \!             { \s -> TChannel body }
--  \$             { \s -> TChannel "bearing" }
--  \&             { \s -> TChannel "tangling" }

  "/"              { \s -> TSlash }
  "|"            {  \s -> TPipe }
  "("            {  \s -> TOpen }
  ")"            {  \s -> TClose }
  "["            {  \s -> TOpenS }
  "]"            {  \s -> TCloseS }
  \n          { \s -> TNewline}
  $spaces+   ;--{ \s -> TSpaces  }
  --$actions     { action $ s }
  \*            { action $ (State (\c-> (("step", 1), feet))) }
  "+"            { multiChannel feet "free odd steps" 2 [(body, "change weigth", 2)] }
  "-"            { multiChannel feet "free even steps" 2 [(body, "nochange weigth", 2)] }
  ";"            { newAction feet "stamp"  1 }
  ";/-"            { newAction feet "stomp"  1 }
  \,             { newAction feet "heel" (1) }
  \'             { newAction feet "touch" (1) }
  \"             { newAction feet "shuffle" (1) }
  \:            { action $ (return ("",1)) }
  \.            { action $ (return ("",1/2)) } -- to stop an action
  "^"            { action $ (State action_up) }
  ">"            { action $ (State action_right) }
  v            { action $ (State action_down) }
  \<            { action $ (State action_left) }
  \{             { multiChannel feet "ending left" 2 [(body, "push left" ,2)] }
  \}             { multiChannel feet "ending right" 2 [(body, "push right" ,2)] }
  \%             { multiChannel feet "rock step" 2 [(body, "push push",2) ] }
  \;              { newAction feet "step step " 1}
  o             { newAction feet "hold" 1 }
  --\(\@\@\)             { newAction body "double spin" 2 } -- @@ or (@@) 
  \@             { newAction body "spin" 2 }
  \?             { multiChannel body "1/2 turn" 2 [(feet, "hook back", 1)] } -- ^!? after a ^
  --(\?\?)             { newAction body "1/4 turn" 2 } -- (??)
\([\<>\{\}\+\-]?[\?\@]+\)  { \s -> TAction (State (\c -> ((S.parse s, 2), body)))  } -- ?? = 1/4 <> spin left/right +- spin outward/indward (outward = to the left on left foot)

  x     { newAction feet "cross step" 1 } -- pieds croises a cote
  t     { newAction feet "toe" 1 } 
  h     { newAction feet "hop" 1 } 
  _     { multiChannel body "weight middle" 2 [(feet, "both feet", 2)] } -- 
  =     { multiChannel body "freeze" 2 [(feet, "freeze", 2)] } -- 
  z     { newAction feet "cross forward" 1 } -- 
  X     { newAction body "X stanse" 2 } -- crossed feet
  A     { newAction body "A stanse" 2 } -- pied ecarte
  M     { newAction body "M stanse" 2 } -- pieds sous les epaules
  I     { newAction body "I stanse" 2 } -- pieds joints
  w     { newAction body "feet outward" 2 } -- mi
  m     { newAction body "feet inward" 2 } -- mi
  s     { newAction body "fold" 2 } -- strong compression, before or jump or stop
  S     { newAction body "bend" 2 }
  \~     { newAction body "slide" 0 } -- and at the same time as the final action
  \~\~     { newAction body "Jump" 0 } -- % s ~~=

 
  "0"            { multiChannel feet "still" 2 [(body, "still", 2)] }
  "1"            { multiChannel feet "one step" 2 [(body, "switch weight", 2)] }
  "2"            { multiChannel feet "double steps" 2 [(body, "push push", 2)] }
  "3"            { multiChannel feet "triple steps" 2 [(body, "3 push", 2)] }

{
type Action = (String, Rational)
type MAction = State String Action

newAction :: String -> String -> Rational -> String -> Token
newAction channel action length s  = TAction (State (\c -> ((action, length), channel)))

action :: MAction -> a -> Token
action m s =  TAction m

multiChannel :: String ->  String -> Rational -> [(String, String, Rational)] -> String -> Token
multiChannel channel action length actions s =  TAction (State findIt ) where
    findIt  :: String -> (Action, String)
    findIt c = case (M.lookup c  m ) of 
                  Just a ->  a
                  Nothing -> ((action, length), channel) 
    m = M.fromList (map (\(c, a, l) -> (c, ((a,l), c))) actions) :: M.Map String (Action, String)

feet = "Feet"
body = "Body"
orientation = "Orientation"
tangling = "Tangling"

data Token = TChannel String  |
             TLabel String |
             TSlash           |
             TPipe            |
             TOpen | TClose   |
             TOpenS | TCloseS |
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
