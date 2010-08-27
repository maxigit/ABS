module Tracker where
import Control.Monad.Writer
import Maybe
import Data.List(intercalate)

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
  show (Writer (a, w)) =  (show a) ++ " => " ++ (show w)
--we want command to be sorted by subchannel
--no sub channel for now data  CommandSet = CommandSet { subchannel :: (Maybe String), commands :: CommandSet }

write :: (Monoid w) => w -> (Writer w a) -> (Writer w a)
write w m = do 
  a <- m
  Writer (a, w)

delayed :: Rational -> Event -> Event
delayed d e = write (Sum d) e


-- A track is a transposed score
type Track = [(Beat, CommandGroup)] -- should be sorted
type Score = [(Channel, Track)]

{-data Tracker = Tracker Map-}
type CommandGroup = [Command]
type Line = [(Channel, CommandGroup )]
newtype Tracker = Tracker { tLines :: [(Beat, Line)] }


-- The main function !. Transpose a list of Tracks (Score)  to a list of lines (Tracker)
scoreToTracker :: Score -> Tracker
scoreToTracker [] = Tracker []
scoreToTracker sc | beats == [ ] = Tracker []
                    | True = Tracker $ (b, line) : (tLines (scoreToTracker sc')) where
  channels = [ c | (c, t)  <- sc ]       :: [Channel]
  tracks =  [ t | (c, t)  <- sc ]      :: [Track]
  beats = [ b | (b, g) <-  catMaybes (map listToMaybe tracks) ]     :: [Beat]
  b = foldl1 min beats
  prunes = map (prune) tracks
  prune :: Track ->  (CommandGroup, Track)
  prune [] = ([], [])
  prune all@((b', g):ts) | b' == b = ( g, ts)
                         | True = ([], all)
  line = zip channels (map (\(g, t) -> g) prunes)
  sc' = zip channels (map (\(g, t) -> t) prunes)

showTracker :: Tracker -> String
showTracker (Tracker []) = ""
showTracker (Tracker ((b, line):bls)) = "#" ++ (show (fromRational b :: Float)) ++ ":\t" ++ (showLine line) ++ "\n" ++ (showTracker (Tracker bls))

showLine :: Line -> String
showLine [] = []
showLine ((ch, gs):line) = (ch) ++ ":\t" ++ (intercalate ", " (map show' gs)) ++ "\t" ++ (showLine line) where
  show' (Writer (c, args)) = intercalate " " (c:args)

instance Show Tracker where
  show  t = showTracker t

addBlankLine :: Beat -> Tracker -> Tracker
addBlankLine b t = _addBlankLine 1 b t

_addBlankLine :: Beat -> Beat -> Tracker -> Tracker
_addBlankLine _ _ (Tracker []) = Tracker []
_addBlankLine pos st t@(Tracker (l:ls)) | pos == start = Tracker  $ l  : (tLines ( _addBlankLine (pos + st) st (Tracker ls)))
                                     | pos < start = Tracker $ (pos, [(ch, []) | ch <- channels])  : (tLines ( _addBlankLine (pos+st) st (Tracker (l:ls))))
    where
    channels = getChannels t
    (start, _) = l

getChannels :: Tracker -> [Channel]
getChannels (Tracker []) = []
getChannels (Tracker (l:ls)) = [c | (c,_) <- firstLine ] where
  (b, firstLine ) = l

