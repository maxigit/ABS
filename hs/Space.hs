module Space where
import Data.Monoid
data Bearing = North | East | South | West deriving (Show, Eq, Enum);

data Direction = Front | Right | Back | Left deriving (Show, Eq, Enum);

instance Monoid Direction where
  mempty = Front
  mappend a b = let sum = (fromEnum a) + (fromEnum b)
                in toEnum (mod sum 4)

data Height = Floor | Low | Middle | High | Brush; -- touch, kick 

data Warding = Forward | Inward | Backward | Outward;
instance Monoid Warding where
  mempty = Front
  mappend a b = let sum = (fromEnum a) + (fromEnum b)
                in toEnum (mod sum 4)
