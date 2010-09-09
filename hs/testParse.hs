import HappyParser
import Tracker
import HappyParser



main = do 
  l <- getContents
  print $ addBlankLine 0.5 ((scoreToTracker . parseScore) (l ++ ""))
  {-print $ addBlankLine 0.5 ((scoreToTracker . parseScore) (l ++ "\n"))-}
