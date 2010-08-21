module Main where
import TrackAcc
import Tracker

a :: TrackAcc
a = pushEvent emptyAcc "ch1" ( newEvent ( newCommand "command 1")) 5
main = do
  print "a= " ++ (show a)
  
