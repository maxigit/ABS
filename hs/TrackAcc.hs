module TrackAcc where
import Data.Map(Map)
import qualified Data.Map as M
import Tracker
import Data.Monoid
import Data.List(groupBy)
import Control.Monad.Writer

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
pushCommand (TrackAcc m) ch com del = TrackAcc $ M.insertWith (flip mappend) ch v m where
  v = TrackState del del [newEvent com]

-- add an commmand to Accumutor at the specified position, no update of the inse`t point
insertCommand :: TrackAcc -> Channel -> Command -> Beat -> TrackAcc
insertCommand (TrackAcc m) ch com p = TrackAcc $ M.insertWith (flip mergeTrack ) ch v m where
  v = TrackState 0 0 [event com p]

getTrackState :: Channel -> TrackAcc -> (Maybe TrackState)
getTrackState ch (TrackAcc m) = M.lookup ch m

updateTrackState :: (TrackState -> TrackState) -> Channel ->  TrackAcc -> TrackAcc
updateTrackState f ch (TrackAcc m) = TrackAcc ( M.adjust f ch m)

shiftInsertPoint :: Beat -> Channel -> TrackAcc -> TrackAcc
shiftInsertPoint d ch acc = updateTrackState f ch acc where
  f (TrackState l i es) = TrackState l (i+d) es

insertCommandAndShiftChannel :: TrackAcc -> Channel -> Channel -> Command -> Beat -> TrackAcc
insertCommandAndShiftChannel acc chi chs c l = shiftInsertPoint l chs (insertCommand acc chi c p ) where
    p = case  (getTrackState chs acc) of
          Nothing -> 0
          Just t -> insertPoint t

instance Monoid TrackAcc where
  mempty = TrackAcc M.empty 
  mappend  (TrackAcc a) (TrackAcc b) = TrackAcc (M.unionWith mappend a b)

emptyAcc :: TrackAcc
emptyAcc = mempty

mergeAcc :: TrackAcc -> TrackAcc -> TrackAcc
mergeAcc (TrackAcc a) (TrackAcc b) = TrackAcc (M.unionWith mergeTrack a b )

accToSubScore :: [Channel] -> TrackAcc -> Score
accToSubScore chs acc = [ (ch, groupTrack (getTrackState ch acc))  | ch <- chs ] where
  groupTrack Nothing = []
  groupTrack (Just ts) = map group (groupBy myeq evs) where
      evs = events ts
  myeq e e' = (readWriterLog e ) == (readWriterLog e')
  group (w:ws) = (getSum (readWriterLog w) + 1, map readWriter (w:ws))

accToScore :: TrackAcc -> Score
accToScore acc@(TrackAcc m) = accToSubScore (M.keys m) acc

