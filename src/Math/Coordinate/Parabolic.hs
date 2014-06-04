{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-}

module Math.Coordinate.Parabolic where

import qualified Math.Coordinate.Cartesian as Cartesian
import           Math.Coordinate.Cartesian    (Cartesian)
import           Math.Coordinate.Coordinate   (CoordConversion(..), ManualConversion(..), convertCoord)
import           Math.Space.Space             (Space2)

data Parabolic = Parabolic deriving (Show)

data Point a = Point { rho :: !a
                     , tau :: !a 
                     } deriving (Show)

toParabolic = convertCoord Parabolic

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Floating a => CoordConversion ManualConversion Cartesian space (Point a) (Cartesian.Point2 a) where
    convertCoordBase _ _ _ (Point rho tau) = Cartesian.Point2 x y where
        x = rho * tau
        y = (rho**2 - tau**2)/2

instance Floating a => CoordConversion ManualConversion Parabolic space (Cartesian.Point2 a) (Point a) where
    convertCoordBase _ _ _ (Cartesian.Point2 x y) = Point rho tau
    	where rho = sqrt $ y + (sqrt $ x**2 + y**2)
    	      tau = x/rho

