{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-}

module Math.Coordinate.Polar where

import qualified Math.Coordinate.Cartesian as Cartesian
import           Math.Coordinate.Cartesian    (Cartesian)
import           Math.Coordinate.Coordinate   (CoordConversion(..), ManualConversion(..), convertCoord)
import           Math.Space.Space             (Space2)

data Polar      = Polar deriving (Show)

data Point2 a = Point2 { r   :: !a
                       , phi :: !a
                       } deriving (Show)

toPolar = convertCoord Polar

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Floating a => CoordConversion ManualConversion Cartesian space (Point2 a) (Cartesian.Point2 a) where
    convertCoordBase _ _ _ (Point2 r phi) = Cartesian.Point2 (r * cos phi) (r * sin phi)

instance RealFloat a => CoordConversion ManualConversion Polar space (Cartesian.Point2 a) (Point2 a) where
    convertCoordBase _ _ _ (Cartesian.Point2 x y) = Point2 r phi
        where r   = sqrt $ (x*x) + (y*y)
              phi = atan2 y x