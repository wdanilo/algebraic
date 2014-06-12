{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Math.Coordinate.Elliptic where

import           Data.Typeable                 (Typeable)
import           Control.Applicative

import           Data.Complex
import qualified Math.Coordinate.Cartesian as Cartesian
import           Math.Coordinate.Cartesian    (Cartesian)
import           Math.Coordinate.Coordinate   (CoordConversion(..), ManualConversion(..), convertCoord)
import           Math.Space.Space             (Space2)

data Elliptic a = Elliptic !a deriving (Show)

data Point a = Point { a :: !a
                     , u :: !a
                     , v :: !a
                     } deriving (Eq, Ord, Show, Read, Typeable)

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

--------------------------------------------------------------------------------
-- Point
--------------------------------------------------------------------------------
instance Functor Point where
    fmap f (Point a b c) = Point (f a) (f b) (f c)

instance Applicative Point where
    pure a = Point a a a
    {-# INLINE pure #-}
    Point a b c <*> Point d e f = Point (a d) (b e) (c f)
    {-# INLINE (<*>) #-}

instance Num a => Num (Point a) where
    (+) = liftA2 (+)
    {-# INLINE (+) #-}
    (-) = liftA2 (-)
    {-# INLINE (-) #-}
    (*) = liftA2 (*)
    {-# INLINE (*) #-}
    negate = fmap negate
    {-# INLINE negate #-}
    abs = fmap abs
    {-# INLINE abs #-}
    signum = fmap signum
    {-# INLINE signum #-}
    fromInteger = pure . fromInteger
    {-# INLINE fromInteger #-}

instance Fractional a => Fractional (Point a) where
    recip = fmap recip
    {-# INLINE recip #-}
    (/) = liftA2 (/)
    {-# INLINE (/) #-}
    fromRational = pure . fromRational
    {-# INLINE fromRational #-}

