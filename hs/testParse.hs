import HappyParser
import Tracker
import HappyParser


strToTrack l = addBlankLine 0.5 ((scoreToTracker . parseScore) l)

main = do 
  l <- getContents
  print (strToTrack  (l ++ ""))
  {-print $ addBlankLine 0.5 ((scoreToTracker . parseScore) (l ++ "\n"))-}
