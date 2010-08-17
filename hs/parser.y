{
module HappyParser where
import Lexer
}

%name parser  commands
%tokentype { Token}
%error { parseError}

%token
  title  { TLabel $$ }
  NL     { TNewline }
  SP     { TSpaces }
  tcommand { TCommand $$ }
  tselector { TChannel $$ }
  

%%
commands : command { [ $1 ] }
         | command commands { $1:$2 } 


command : tcommand { PCommand $1 Nothing }
 | tselector tcommand { PCommand $2 (Just $1) }

selector : tselector { TChannel $1 }




{
data PCommand = PCommand {command :: String, channel :: (Maybe String)} deriving (Show, Eq)
-- main = getContents >>= print . parser . lexer
parse = parser.lexer
parseError :: [Token] -> a
parseError _ = error "Parse error"
-- a la fin
}
