{
module HappyParser where
import Lexer
}

%name abs
%tokentype { Token}
%error { ParseError}

%token
  title  { TString $$ }
  NL     { TNewline }
  SP     { TSpaces }
  command { TCommand String }
  selector { TChannel $$ }}
  

%%
block : title 
        lines

lines : line             { [$1] }
        | line lines     { $1 : $2 }

line : commands NL 

commands : metacommand SP commands

metacommand : command
              | selector command

selected_commands : selector SP commands





{
-- a la fin
}
