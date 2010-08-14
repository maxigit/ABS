module Tracker where


-- Command : what is sent to channel
data Command = Command { name :: String, args :: [String] } deriving (Show, Eq)

-- we want command to be sorted by subchannel
data CommandSet = CommandSet { subchannel :: (Maybe String), commands :: [Command] }

-- time
data Beat = Beat [Int] deriving (Show, Eq)
data Time = Time { bar :: Int,  beat :: Beat}

-- Tracker line , a time _ a a list of command for each track
data Line = Line Time [Track [CommandSet]];
