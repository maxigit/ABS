{
module HappyParser where
import Lexer
import Control.Monad.State
}

%name parser   
%tokentype { Token}
%error { parseError}

%token
  title  { TLabel $$ }
  NL     { TNewline }
  SP     { TSpaces }
  tcommand { TCommand $$ }
  tselector { TChannel $$ }
  

%%
-- set of command without separators

channel_sequence : tselector SP words { map  (run $1) (concat $3) }
words : word { [$1] }
      | word SP words { $1:$3 }
word : command { [ $1 ] }
         | command word { $1:$2 } 


command : tcommand { TCommand $1 }
 | tselector tcommand { TCommand $ State (\c -> runState $2 $1)  }

selector : tselector { $1 }




{
data PCommand = PCommand {command :: String, channel :: (Maybe String)} deriving (Show, Eq)
data PSequence = PSequence { channel2 :: (Maybe String), commands :: [[PCommand]] } deriving (Show, Eq)
-- main = getContents >>= print . parser . lexer
parse = parser.lexer
parseError :: [Token] -> a
parseError _ = error "Parse error"
-- a la fin
}
