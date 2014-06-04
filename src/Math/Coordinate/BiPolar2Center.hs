{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-}

module Math.Coordinate.BiPolar2Center where

import qualified Math.Coordinate.Cartesian as Cartesian
import           Math.Coordinate.Cartesian    (Cartesian)
import           Math.Coordinate.Coordinate   (CoordConversion(..), ManualConversion(..), convertCoord)
import           Math.Space.Space             (Space2)

data BiPolar2Center a = BiPolar2Center !a deriving (Show)

data Point a = Point { a  :: !a
                     , r1 :: !a
                     , r2 :: !a
                     } deriving (Show)

toBiPolar2Center = convertCoord . BiPolar2Center

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Floating a => CoordConversion ManualConversion Cartesian space (Point a) (Cartesian.Point2 a) where
    convertCoordBase _ _ _ (Point a r1 r2) = Cartesian.Point2 x y where
        x = (r1**2 - r2**2) / (4*a)
        y = (sqrt $ 16 * a**2 * r1**2 - (r1**2 - r2**2 + 4 * a**2))/(4*a)

instance (Floating a, a~b) => CoordConversion ManualConversion (BiPolar2Center b) space (Cartesian.Point2 a) (Point a) where
    convertCoordBase _ (BiPolar2Center a) _ (Cartesian.Point2 x y) = Point a r1 r2
        where r1 = sqrt $ (a+x)**2 + y**2
              r2 = sqrt $ (x-a)**2 + y**2