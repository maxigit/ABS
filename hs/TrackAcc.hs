module TrackAcc where
import Data.Map
import Tracker
import Data.Monoid

{-TrackAcc is used by the Parser and store , all the intermediatary states need to parse it-}
data TrackState = TrackState { lastBeat :: Beat, insertPoint :: Beat, events :: [Event]}
data TrackAcc = TrackAcc (Map Channel  TrackState )

-- TrackAcc
instance Monoid TrackState where
  mempty = TrackState 0 0 []
{-TODO shift event os es'-}
  mappend (TrackState l i es) (TrackState l' i' es') = TrackState (l+l') (i+i') (es++es'') where 
    es'' = Prelude.map (delayed i) es'

--addTrack = reduce pushEventToTra
-- make Event a monad to manage the time => Timed , we might to add the duration
pushEvent :: TrackAcc -> Channel -> Event -> Beat -> TrackAcc
pushEvent (TrackAcc m) ch ev d = TrackAcc $ insertWith mappend ch v m where
  v = TrackState d d [ev]

{-
 -instance Monoind TrackAcc where
 -  mempty = Track empty 
 -  mappend  
 -}
