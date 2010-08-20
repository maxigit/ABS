module Tracker where
import Control.Monad.Writer

type Channel = String
type Beat = Rational

--Command : what is sent to channel
data Command = Command { name :: String, args :: [String] } deriving (Show, Eq)

{-data Event = Event { beat :: Beat, command ::  Command } deriving (Show, Eq)-}
type RationalM = Sum Rational
newtype Event =  Event (Writer RationalM Command)

instance Show Event  where
  show (Event ev) = (show d) ++ " : " ++ (show c) where (c, d) = runWriter ev
--we want command to be sorted by subchannel
--no sub channel for now data  CommandSet = CommandSet { subchannel :: (Maybe String), commands :: CommandSet }



type Track = [Event]

{-data Tracker = Tracker Map-}


