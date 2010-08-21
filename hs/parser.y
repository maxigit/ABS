{
module HappyParser where
import Lexer
import TrackAcc
import Tracker
import Control.Monad.State
}

%name parser   
%tokentype { Token}
%error { parseError}

%token
  title  { TLabel $$ }
  NL     { TNewline }
  SP     { TSpaces }
  taction { TAction $$ }
  tselector { TChannel $$ }
  

%%
-- set of action without separators

channel_sequence : tselector SP words { addActions $1  emptyAcc (concat $3) }
words : word { [$1] }
      | word SP words { $1:$3 }
word : action { [ $1 ] }
         | action word { $1:$2 } 


action : taction { $1 }
 | tselector taction { State (\c -> runState $2 $1)  }

selector : tselector { $1 }




{

addActions :: Channel -> TrackAcc -> [MAction] -> TrackAcc
addActions ch acc acs  = foldl push acc (map f acs) where
     push acc' (ch', c, l)  = pushCommand acc' ch' c l
     f m = (ch', return a , l) where ((a, l), ch') = runState m ch

data PAction = PAction {action :: String, channel :: (Maybe String)} deriving (Show, Eq)
data PSequence = PSequence { channel2 :: (Maybe String), actions :: [[PAction]] } deriving (Show, Eq)

-- main = getContents >>= print . parser . lexer
parse = parser.lexer
parseError :: [Token] -> a
parseError _ = error "Parse error"
-- a la fin
}
