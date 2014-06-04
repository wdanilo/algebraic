{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-}

module Math.Coordinate.UV where

import qualified Math.Coordinate.Cartesian  as Cartesian
import           Math.Coordinate.Cartesian     (Cartesian)
import           Math.Coordinate.Coordinate    (CoordConversion(..), ManualConversion(..), convertCoord)
import qualified Math.Space.Space           as Space
import           Math.Space.Space              (Space2)

data UV         = UV deriving (Show)
data UPoint   a = UPoint !a deriving (Show)
data UVPoint  a = UVPoint !a !a deriving (Show)
data UVWPoint a = UVWPoint !a !a !a deriving (Show)

toUV = convertCoord UV

--------------------------------------------------------------------------------
-- Classes
--------------------------------------------------------------------------------

class UVCoord1 coord where
    u :: coord a -> a

class UVCoord1 coord => UVCoord2 coord where
    v :: coord a -> a

class UVCoord2 coord => UVCoord3 coord where
    w :: coord a -> a

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance UVCoord1 UPoint where u (UPoint u) = u

instance UVCoord1 UVPoint where u (UVPoint u _) = u
instance UVCoord2 UVPoint where v (UVPoint _ v) = v

instance UVCoord1 UVWPoint where u (UVWPoint u _ _) = u
instance UVCoord2 UVWPoint where v (UVWPoint _ v _) = v
instance UVCoord3 UVWPoint where w (UVWPoint _ _ w) = w


instance (Space2 space, Num a, a~b) => CoordConversion ManualConversion Cartesian (space b) (UVPoint a) (Cartesian.Point2 a) where
    convertCoordBase _ _ space (UVPoint u v) = Cartesian.Point2 (u*w) (v*h)
        where w = Space.width  space
              h = Space.height space

instance (Space2 space, Fractional a, a~b) => CoordConversion ManualConversion UV (space b) (Cartesian.Point2 a) (UVPoint a) where
    convertCoordBase _ sys space (Cartesian.Point2 x y) = UVPoint (x/w) (y/h)
        where w = Space.width  space
              h = Space.height space