{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Math.Coordinate.LogPolar where

import           Data.Typeable                 (Typeable)
import           Control.Applicative

import qualified Math.Coordinate.Cartesian as Cartesian
import           Math.Coordinate.Cartesian    (Cartesian)
import           Math.Coordinate.Coordinate   (CoordConversion(..), ManualConversion(..), convertCoord)
import           Math.Space.Space             (Space2)
import qualified Math.Constants            as Const

data LogPolar = LogPolar deriving (Show)

data Point2 a = Point2 { rho :: !a
                       , phi :: !a
                       } deriving (Eq, Ord, Show, Read, Typeable)

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

--------------------------------------------------------------------------------
-- Point2
--------------------------------------------------------------------------------
instance Functor Point2 where
    fmap f (Point2 a b) = Point2 (f a) (f b)

instance Applicative Point2 where
    pure a = Point2 a a
    {-# INLINE pure #-}
    Point2 a b <*> Point2 d e = Point2 (a d) (b e)
    {-# INLINE (<*>) #-}

instance Num a => Num (Point2 a) where
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

instance Fractional a => Fractional (Point2 a) where
    recip = fmap recip
    {-# INLINE recip #-}
    (/) = liftA2 (/)
    {-# INLINE (/) #-}
    fromRational = pure . fromRational
    {-# INLINE fromRational #-}
