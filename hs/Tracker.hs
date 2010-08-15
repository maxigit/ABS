module Tracker where


--Command : what is sent to channel
data Command = Command { name :: String, args :: [String] } deriving (Show, Eq)
type CommandSet = [Command]

--we want command to be sorted by subchannel
--no sub channel for now data  CommandSet = CommandSet { subchannel :: (Maybe String), commands :: CommandSet }

--time
data Beat = Beat [Int] deriving (Show, Eq)
data Time = Time Integer deriving (Show , Eq)

--Tracker line , a time _ a a list of command for each track
data Line = Line Time [Track CommandSet]

data Track = [Time CommandSet]
