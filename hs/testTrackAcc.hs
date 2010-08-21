module Main where
import TrackAcc
import Tracker
import Data.Monoid -- to ues mappend

a :: TrackAcc
a = pushEvent emptyAcc "ch1" ( newEvent ( newCommand "command 1.1")) 2
b = pushEvent emptyAcc "ch1" ( newEvent ( newCommand "command 1.2")) 5
c = pushEvent emptyAcc "ch2" ( newEvent ( newCommand "command 2.1")) (1 / 2)
main = do
  putStrLn $ "a= " ++ (show a)
  putStrLn $ "b= " ++ (show b)
  putStrLn $ "c= " ++ (show c)
  putStrLn ""
  putStrLn $ "a+b+c" ++ (show ( a `mappend` b `mappend` c))
  
