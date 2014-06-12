{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Math.Coordinate.BiPolar2Center where

import           Data.Typeable                 (Typeable)
import           Control.Applicative

import qualified Math.Coordinate.Cartesian as Cartesian
import           Math.Coordinate.Cartesian    (Cartesian)
import           Math.Coordinate.Coordinate   (CoordConversion(..), ManualConversion(..), convertCoord)
import           Math.Space.Space             (Space2)

data BiPolar2Center a = BiPolar2Center !a deriving (Show)

data Point a = Point { a  :: !a
                     , r1 :: !a
                     , r2 :: !a
                     } deriving (Eq, Ord, Show, Read, Typeable)

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
