{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-}

module Math.Coordinate.LogPolar where

import qualified Math.Coordinate.Cartesian as Cartesian
import           Math.Coordinate.Cartesian    (Cartesian)
import           Math.Coordinate.Coordinate   (CoordConversion(..), ManualConversion(..), convertCoord)
import           Math.Space.Space             (Space2)
import qualified Math.Constants            as Const

data LogPolar = LogPolar deriving (Show)

data Point2 a = Point2 { rho :: !a
                       , phi :: !a
                       } deriving (Show)

toLogPolar = convertCoord LogPolar

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance RealFloat a => CoordConversion ManualConversion Cartesian space (Point2 a) (Cartesian.Point2 a) where
    convertCoordBase _ _ _ pt@(Point2 rho phi) = Cartesian.Point2 (base * cos phi) (base * sin phi)
        where base = Const.e**rho


instance RealFloat a => CoordConversion ManualConversion LogPolar space (Cartesian.Point2 a) (Point2 a) where
    convertCoordBase _ _ _ (Cartesian.Point2 x y) = Point2 rho phi
        where rho = log . sqrt $ (x*x) + (y*y)
              phi = atan2 y x