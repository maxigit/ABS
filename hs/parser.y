{
module HappyParser where
import Lexer
}

%name parser
%tokentype { Token}
%error { parseError}

%token
  title  { TLabel $$ }
  NL     { TNewline }
  SP     { TSpaces }
  command { TCommand $$ }
  selector { TChannel $$ }
  

%%
commands : command { [TCommand $1 ] }
         | command commands {   (TCommand $1):$2}





{
-- main = getContents >>= print . parser . lexer
parse = parser.lexer
parseError :: [Token] -> a
parseError _ = error "Parse error"
-- a la fin
}
