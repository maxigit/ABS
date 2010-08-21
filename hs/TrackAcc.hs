module TrackAcc where
import Data.Map
import Tracker
import Data.Monoid

{-TrackAcc is used by the Parser and store , all the intermediatary states need to parse it-}
data TrackState = TrackState { lastBeat :: Beat, insertPoint :: Beat, events :: [Event]} deriving (Show)
data TrackAcc = TrackAcc (Map Channel  TrackState ) deriving (Show)

-- TrackAcc
instance Monoid TrackState where
  mempty = TrackState 0 0 []
{-TODO shift event os es'-}
  mappend (TrackState l i es) (TrackState l' i' es') = TrackState (l+l') (i+i') (es++es'') where 
    es'' = Prelude.map (delayed i) es'

-- merge to track without shiftind any time  o events
mergeTrack :: TrackState -> TrackState -> TrackState
mergeTrack (TrackState l i es) (TrackState l' i' es') = TrackState (l) (i) (es++es') where 

emptyState :: TrackState
emptyState = mempty

--addTrack = reduce pushEventToTra
-- add an command to Accumutor and update instert point accordingly to the length
pushCommand :: TrackAcc -> Channel -> Command -> Beat -> TrackAcc
pushCommand (TrackAcc m) ch com del = TrackAcc $ insertWith mappend ch v m where
  v = TrackState del del [newEvent com]

-- add an commmand to Accumutor at the specified position, no update of the inse`t point
insertCommand :: TrackAcc -> Channel -> Command -> Beat -> TrackAcc
insertCommand (TrackAcc m) ch com p = TrackAcc $ insertWith (flip mergeTrack ) ch v m where
  v = TrackState 0 0 [event com p]

instance Monoid TrackAcc where
  mempty = TrackAcc empty 
  mappend  (TrackAcc a) (TrackAcc b) = TrackAcc (unionWith mappend a b)

emptyAcc :: TrackAcc
emptyAcc = mempty
