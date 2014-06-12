---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Math.Space.Space where

--data Space2 = Space2 !Double !Double deriving (Show)

class Space2 space where
	width  :: space a -> a
	height :: space a -> a

class Space2 space => Space3 space where
	depth :: space a -> a

data Grid a = Grid !a !a deriving (Show)

instance Space2 Grid where
	width  (Grid x _) = x
	height (Grid _ x) = x
