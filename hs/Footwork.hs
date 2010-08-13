module Footwork where
import Space as S
-- representne the different footwork


-- Move are transition between state, even though often, a transition only specify the destination state
data Step = Rest        |  -- . : ::
            Step           |  -- , *
            Cross          |  -- x X
            Touch          |  -- '
            Shuffle        |  -- " (flap pull back, etc, will be using modifier)
            Kick           |  -- < ^ > v
            Hold           | -- o 
            Both           |
            Jump          
            deriving (Show, Eq, Enum)
--Jump, cross,  A h it etc ? do we need a proper move hierachy ?
--we don't want the semantic, or anything, we just need to know wher the body weight is = left right or unknow

weight :: Step -> (Maybe Warding) -> (Maybe Warding)
-- weight transfering move
weight Step  = swapWeight
weight Cross  = swapWeight

-- two feet move
weight Both _ = Nothing
weight Jump _ = Nothing
weight _ w = w -- default case keep the same

swapWeight (Just Inward) = Outward;
swapWeight (Just Outward) = Inward;
swapWeight _ = Nothing;


data FW = FW Step (Maybe Warding) deriving (Show, Eq) -- the warding is for the weight , not a body displacement !!!


