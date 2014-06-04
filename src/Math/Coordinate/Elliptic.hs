{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-}

module Math.Coordinate.Elliptic where

import           Data.Complex
import qualified Math.Coordinate.Cartesian as Cartesian
import           Math.Coordinate.Cartesian    (Cartesian)
import           Math.Coordinate.Coordinate   (CoordConversion(..), ManualConversion(..), convertCoord)
import           Math.Space.Space             (Space2)

data Elliptic a = Elliptic !a deriving (Show)

data Point a = Point { a :: !a
                     , u :: !a
                     , v :: !a
                     } deriving (Show)

toElliptic = convertCoord . Elliptic

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Floating a => CoordConversion ManualConversion Cartesian space (Point a) (Cartesian.Point2 a) where
    convertCoordBase _ _ _ (Point a u v) = Cartesian.Point2 x y where
        x = a * (cosh u * cos v)
        y = a * (sinh u * sin v)

instance (Floating a, a~b) => CoordConversion ManualConversion (Elliptic b) space (Cartesian.Point2 a) (Point a) where
    convertCoordBase _ (Elliptic a) _ (Cartesian.Point2 x y) = Point a u v
        where u = acosh $ -sqrt(2)*x / (sqrt $ 1 + x**2 + y**2 - (sqrt $ 4*y**2+ (-1 + x**2 + y**2)**2))
              v = acos $ -sqrt(1 + x**2 + y**2 - (sqrt $ 4*y**2 + (-1 + x**2 + y**2)**2)) / (sqrt 2)