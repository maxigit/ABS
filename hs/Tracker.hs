module Tracker where

type Channel = String
type Beat = Rational

--Command : what is sent to channel
data Command = Command { name :: String, args :: [String] } deriving (Show, Eq)

data Event = Event Beat Command deriving (Show, Eq)
--we want command to be sorted by subchannel
--no sub channel for now data  CommandSet = CommandSet { subchannel :: (Maybe String), commands :: CommandSet }


data Track = Track { lastBeat :: Beat, events :: [Event]}
