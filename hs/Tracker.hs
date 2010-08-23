module Tracker where
import Control.Monad.Writer

type Channel = String
type Beat = Rational

--Command : what is sent to channel
--data Command = Command { name :: String, args :: [String] } deriving (Show, Eq)
type Command = Writer [String] String
newCommand :: String -> Command
newCommand s = return s

{-data Event = Event { beat :: Beat, command ::  Command } deriving (Show, Eq)-}
type RationalM = Sum Rational
type Event =  Writer RationalM Command
newEvent :: Command -> Event
newEvent c = return c

event :: Command -> Beat -> Event
event com b = Writer (com, (Sum b))

readWriter :: Writer a b -> b
readWriter w = a where (a, b) = runWriter w

readWriterLog :: Writer a b -> a
readWriterLog w = b where (a, b) = runWriter w
{-instance Show Event  where-}
  {-show (Event ev) = (show d) ++ " : " ++ (show c) where (c, d) = runWriter ev-}

instance (Show w, Show a) => Show (Writer w a) where
  show (Writer (a, w)) =  (show w) ++ " : " ++ (show a)
--we want command to be sorted by subchannel
--no sub channel for now data  CommandSet = CommandSet { subchannel :: (Maybe String), commands :: CommandSet }

write :: (Monoid w) => w -> (Writer w a) -> (Writer w a)
write w m = do 
  a <- m
  Writer (a, w)

delayed :: Rational -> Event -> Event
delayed d e = write (Sum d) e


type Track = [(Beat, CommandGroup)] -- should be sorted
type Score = [(Channel, Track)]

{-data Tracker = Tracker Map-}
type CommandGroup = [Command]
type Line = [(Channel, CommandGroup )]
type Tracker = [(Beat, Line)]


