module Tracker where
import Control.Monad.Writer

type Channel = String
type Beat = Rational

--Command : what is sent to channel
data Command = Command { name :: String, args :: [String] } deriving (Show, Eq)

{-data Event = Event { beat :: Beat, command ::  Command } deriving (Show, Eq)-}
type RationalM = Sum Rational
type Event =  Writer RationalM Command

{-instance Show Event  where-}
  {-show (Event ev) = (show d) ++ " : " ++ (show c) where (c, d) = runWriter ev-}

instance (Show w, Show a) => Show (Writer w a) where
  show (Writer (a, w)) =  (show w) ++ " : " ++ (show a)
--we want command to be sorted by subchannel
--no sub channel for now data  CommandSet = CommandSet { subchannel :: (Maybe String), commands :: CommandSet }



type Track = [Event]

{-data Tracker = Tracker Map-}


