{
module SpinLexer where
import Data.Ratio
import Data.List
}

%wrapper "basic"
tokens :-
    \( ;
  \)  ;
  "<" { \s  ->  ("right", 0 :: Rational) }
  ">" { \s  ->  ("left", 0 :: Rational) }
  "+" { \s  ->  ("outward", 0 :: Rational) }
  "-" { \s  ->  ("inward", 0 :: Rational) }

  "?" { \s  ->  ("", 1%2 :: Rational)}
  "??" { \s  -> ("", 1%4 :: Rational)}
  "@" { \s  ->  ("", 1 :: Rational)}
  "@@" { \s  ->  ("", 2 :: Rational)}

{
--lexer :: String -> [(String, Rational)]
lexer s = foldl1  aggregate (alexScanTokens s) where
  aggregate (a, b) (a', b') = (a, b+b')

parse s = let (i, f) = divMod (numerator length) (denominator length)
              (direction, length) = lexer s
              spin = case i of
                        0 -> ""
                        1 -> "1 spin"
                        _  -> (show i) ++ " spins"
              turn = (show f) ++ "/" ++ (show (denominator length))
              spinstr= case f of 
                      0 -> spin
                      _ -> case spin of
                            "" -> turn ++ " turn"
                            _  -> spin ++ " " ++ turn
          in intercalate " " (filter (not.null) [spinstr, direction])
}
