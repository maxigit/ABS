import HappyParser
import Tracker
import HappyParser



main = do 
  l <- getLine
  print $ addBlankLine 0.5 ((scoreToTracker . parseScore) l)
