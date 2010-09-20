{
module HappyParser where
import Lexer
import TrackAcc
import Tracker
import Control.Monad.State
import Data.Monoid
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
  "|" {TPipe }
  "(" { TOpen }
  ")" { TClose }
  "[" { TOpenS }
  "]" { TCloseS }
  

%%
-- set of action without separators
-- really ugly we have to store all the lines first,so we can superAdd them form left to right
lines :  { [ ]} 
      | line lines{ $1 : $2 }
line : block tblocks { mergeSuper (SuperAcc feet $1) $2 } 
     | tblocks { $1 }

tblocks : NL { SuperAcc undefined  emptyAcc }
        | tblock tblocks { mergeSuper $1 $2 }
      

 {-Maybe we need a preprocessor to add auto () and defaut channel-}
block : word { addTActions feet  $1 }
tblock : tselector word { SuperAcc $1 (addTActions $1 $2) }

word : actions { $1 }
actions: action { [$1] }
         | action word { $1:$2 } 


actions_list: actions  { [$1] }
            | "|" actions_list { [Single (return ("", 1) )]:$2  }
             | actions "|"  actions_list { $1:$3}
action : taction { Single $1 }
          --| "(" actions ")" { Timed $2 2 }
          | "(" actions_list ")" { Timed [Timed x 1 | x <- $2] 2 }
          | "[" actions "]" { Simultaneous $2  }
--         | tselector taction { State (\c -> runState $2 $1)  }

selector : tselector { $1 }




{
data ActionTree = Single MAction |
                  Timed [ActionTree] Beat |
                  Simultaneous [ActionTree]  

addActions :: Channel -> TrackAcc -> [MAction] -> TrackAcc
addActions ch acc acs  = foldl push acc (map f acs) where
     push acc' (ch', c, l) | ch' == ch  = pushCommand acc' ch' c l
                           | True  = insertCommandAndShiftChannel acc' ch' ch c l
     f m = (ch', return a , l) where ((a, l), ch') = runState m ch

addTActions :: Channel -> [ActionTree] -> TrackAcc
addTActions ch  tacs  = foldl (push ch) emptyAcc tacs  where
     push :: Channel -> TrackAcc -> ActionTree -> TrackAcc
     push ch acc' (Single act) | ch' == ch  = pushCommand acc' ch' c l
                             | True  = insertCommandAndShiftChannel acc' ch' ch c l
                             where ((a, l), ch') = runState act ch
                                   c = return a
     push ch acc (Timed acs length) = mergeAndAdd ch acc (compressAcc length (addTActions ch acs))
     push ch acc (Simultaneous acs ) = mergeAndAdd ch acc 
        ( foldl1 mergeAcc ( map
              (addTActions ch)  (map return  acs)
        ))
            
data PAction = PAction {action :: String, channel :: (Maybe String)} deriving (Show, Eq)
data PSequence = PSequence { channel2 :: (Maybe String), actions :: [[PAction]] } deriving (Show, Eq)

-- main = getContents >>= print . parser . lexer
parse x = acc $ foldl1 addSuper  ((parser.lexer) x)
parseError :: [Token] -> a
parseError _ = error "Parse error"

parseScore = accToScore . parse
-- a la fin
}
-- vim: ft=haskell
