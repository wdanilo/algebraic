module Math.Space.Space where

class Space2 space where
	width  :: space a -> a
	height :: space a -> a

class Space2 space => Space3 space where
	depth :: space a -> a

data Grid a = Grid !a !a deriving (Show)

instance Space2 Grid where
	width  (Grid x _) = x
	height (Grid _ x) = x