module Main where
import TrackAcc
import Tracker
import Data.Monoid -- to ues mappend

a :: TrackAcc
a = pushCommand emptyAcc "ch1" (newCommand "command 1.1") 2
b = pushCommand emptyAcc "ch1" (newCommand "command 1.2") 5
c = pushCommand emptyAcc "ch2" (newCommand "command 2.1") (1 / 2)
abc = a `mappend` b `mappend` c
main = do
  putStrLn $ "a= " ++ (show a)
  putStrLn $ "b= " ++ (show b)
  putStrLn $ "c= " ++ (show c)
  putStrLn ""
  putStrLn $ "a+b+c" ++ (show ( insertCommand abc "ch1" (newCommand "i") 0.25 ))
  
