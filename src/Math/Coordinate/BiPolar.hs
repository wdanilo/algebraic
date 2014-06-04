{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-}

module Math.Coordinate.BiPolar where

import qualified Math.Coordinate.Cartesian as Cartesian
import           Math.Coordinate.Cartesian    (Cartesian)
import           Math.Coordinate.Coordinate   (CoordConversion(..), ManualConversion(..), convertCoord)
import           Math.Space.Space             (Space2)

data BiPolar a = BiPolar !a deriving (Show)

data Point a = Point { a     :: !a
                     , sigma :: !a 
                     , tau   :: !a 
                     } deriving (Show)

toBiPolar = convertCoord . BiPolar

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Floating a => CoordConversion ManualConversion Cartesian space (Point a) (Cartesian.Point2 a) where
    convertCoordBase _ _ _ (Point a sigma tau) = Cartesian.Point2 x y where
        x = a * (sinh tau) / (cosh tau - cos sigma)
        y = a * (sin sigma) / (cosh tau - cos sigma)

instance (a~b) => CoordConversion ManualConversion (BiPolar b) space (Cartesian.Point2 a) (Point a) where
    convertCoordBase _ _ _ (Cartesian.Point2 x y) = undefined