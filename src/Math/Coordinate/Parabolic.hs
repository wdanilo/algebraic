{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Math.Coordinate.Parabolic where

import           Data.Typeable                 (Typeable)
import           Control.Applicative

import qualified Math.Coordinate.Cartesian as Cartesian
import           Math.Coordinate.Cartesian    (Cartesian)
import           Math.Coordinate.Coordinate   (CoordConversion(..), ManualConversion(..), convertCoord)
import           Math.Space.Space             (Space2)

data Parabolic = Parabolic deriving (Show)

data Point a = Point { rho :: !a
                     , tau :: !a 
                     } deriving (Eq, Ord, Show, Read, Typeable)

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

--------------------------------------------------------------------------------
-- Point
--------------------------------------------------------------------------------
instance Functor Point where
    fmap f (Point a b) = Point (f a) (f b)

instance Applicative Point where
    pure a = Point a a
    {-# INLINE pure #-}
    Point a b <*> Point d e = Point (a d) (b e)
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
