{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Math.Coordinate.Cartesian where

import Math.Coordinate.Coordinate (CoordConversion(..), ManualConversion(..), AutoConversion(..), convertCoord)
import Math.Space.Space           (Space2)

data Cartesian  = Cartesian deriving (Show)

data Point1 a   = Point1 !a deriving (Show, Eq)
data Point2 a   = Point2 !a !a deriving (Show, Eq)
data Point3 a   = Point3 !a !a !a deriving (Show, Eq)
data Point4 a   = Point4 !a !a !a !a deriving (Show, Eq)

toCartesian = convertCoord Cartesian

--------------------------------------------------------------------------------
-- Classes
--------------------------------------------------------------------------------

class CartesianCoord1 coord where
    x :: coord a -> a

class CartesianCoord1 coord => CartesianCoord2 coord where
    y :: coord a -> a

class CartesianCoord2 coord => CartesianCoord3 coord where
    z :: coord a -> a

class CartesianCoord3 coord => CartesianCoord4 coord where
    w :: coord a -> a

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance CartesianCoord1 Point1 where x (Point1 x) = x

instance CartesianCoord1 Point2 where x (Point2 x _) = x
instance CartesianCoord2 Point2 where y (Point2 _ y) = y

instance CartesianCoord1 Point3 where x (Point3 x _ _) = x
instance CartesianCoord2 Point3 where y (Point3 _ y _) = y
instance CartesianCoord3 Point3 where z (Point3 _ _ z) = z

instance CartesianCoord1 Point4 where x (Point4 x _ _ _) = x
instance CartesianCoord2 Point4 where y (Point4 _ y _ _) = y
instance CartesianCoord3 Point4 where z (Point4 _ _ z _) = z
instance CartesianCoord4 Point4 where w (Point4 _ _ _ w) = w


instance  ( CoordConversion ManualConversion Cartesian space a b
          , CoordConversion ManualConversion sys space b c) => 
          CoordConversion AutoConversion sys space a c where
    convertCoordBase _ coord space = (convertCoordBase ManualConversion coord space) . (convertCoordBase ManualConversion Cartesian space)


instance CoordConversion ManualConversion Cartesian space (Point2 a) (Point2 a) where
    convertCoordBase _ _ _ = id
