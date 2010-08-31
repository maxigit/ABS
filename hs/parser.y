{
module HappyParser where
import Lexer
import TrackAcc
import Tracker
import Control.Monad.State
 -- for debug
import System.IO.Unsafe
import System.IO
}

%name parser   
%tokentype { Token}
%error { parseError}

%token
  title  { TLabel $$ }
  NL     { TNewline }
  taction { TAction $$ }
  tselector { TChannel $$ }
  pipe  {TPipe }
  

%%
-- set of action without separators
tblocks : tblock NL { $1}
        | tblock tblocks { $1 `mergeAcc` $2 }

block : word { addActions "feet" emptyAcc $1 }
tblock : tselector word { addActions $1  emptyAcc $2 }

word : action { [ $1 ] }
         | action word { $1:$2 } 


action : taction { $1 }
 -- | tselector taction { State (\c -> runState $2 $1)  }

selector : tselector { $1 }




{

addActions :: Channel -> TrackAcc -> [MAction] -> TrackAcc
addActions ch acc acs  = foldl push acc (map f acs) where
     push acc' (ch', c, l) | ch' == ch  = pushCommand acc' ch' c l
                           | True  = insertCommandAndShiftChannel acc' ch' ch c l
     f m = (ch', return a , l) where ((a, l), ch') = runState m ch

data PAction = PAction {action :: String, channel :: (Maybe String)} deriving (Show, Eq)
data PSequence = PSequence { channel2 :: (Maybe String), actions :: [[PAction]] } deriving (Show, Eq)

-- main = getContents >>= print . parser . lexer
parse = parser.lexer
parseError :: [Token] -> a
parseError _ = error "Parse error"

parseScore = accToScore . parse
-- a la fin
}
