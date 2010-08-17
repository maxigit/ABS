{
module HappyParser where
import Lexer
}

%name abswing
%tokentype { Token}
%error { parseError}

%token
  title  { TLabel $$ }
  NL     { TNewline }
  SP     { TSpaces }
  command { TCommand $$ }
  selector { TChannel $$ }
  

%%
commands : command { $1 }





{
main = getContents >>= print . abs. lexer
parseError :: [Token] -> a
parseError _ = error "Parse error"
-- a la fin
}
