{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Math.Coordinate.Cartesian where

import Data.Typeable              (Typeable)
import Control.Applicative

import Math.Coordinate.Coordinate (CoordConversion(..), ManualConversion(..), AutoConversion(..), convertCoord)
import Math.Space.Space           (Space2)

data Cartesian  = Cartesian deriving (Show)

data Point1 a   = Point1 !a deriving (Eq, Ord, Show, Read,Typeable)
data Point2 a   = Point2 !a !a deriving (Eq, Ord, Show, Read,Typeable)
data Point3 a   = Point3 !a !a !a deriving (Eq, Ord, Show, Read,Typeable)
data Point4 a   = Point4 !a !a !a !a deriving (Eq, Ord, Show, Read,Typeable)

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
-- Functions
--------------------------------------------------------------------------------
uncurry :: (a -> a -> b) -> Point2 a -> b
uncurry f (Point2 x y) = f x y

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

--------------------------------------------------------------------------------
-- Point1
--------------------------------------------------------------------------------
instance Functor Point1 where
    fmap f (Point1 a) = Point1 (f a)

instance Applicative Point1 where
    pure a = Point1 a
    {-# INLINE pure #-}
    Point1 a <*> Point1 d = Point1 (a d)
    {-# INLINE (<*>) #-}

instance Num a => Num (Point1 a) where
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

instance Fractional a => Fractional (Point1 a) where
    recip = fmap recip
    {-# INLINE recip #-}
    (/) = liftA2 (/)
    {-# INLINE (/) #-}
    fromRational = pure . fromRational
    {-# INLINE fromRational #-}


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

--------------------------------------------------------------------------------
-- Point3
--------------------------------------------------------------------------------
instance Functor Point3 where
    fmap f (Point3 a b c) = Point3 (f a) (f b) (f c)

instance Applicative Point3 where
    pure a = Point3 a a a
    {-# INLINE pure #-}
    Point3 a b c <*> Point3 d e f = Point3 (a d) (b e) (c f)
    {-# INLINE (<*>) #-}

instance Num a => Num (Point3 a) where
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

instance Fractional a => Fractional (Point3 a) where
    recip = fmap recip
    {-# INLINE recip #-}
    (/) = liftA2 (/)
    {-# INLINE (/) #-}
    fromRational = pure . fromRational
    {-# INLINE fromRational #-}

--------------------------------------------------------------------------------
-- Point4
--------------------------------------------------------------------------------
instance Functor Point4 where
    fmap f (Point4 a b c d) = Point4 (f a) (f b) (f c) (f d)

instance Applicative Point4 where
    pure a = Point4 a a a a
    {-# INLINE pure #-}
    Point4 a b c d <*> Point4 e f g h = Point4 (a e) (b f) (c g) (d h)
    {-# INLINE (<*>) #-}

instance Num a => Num (Point4 a) where
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

instance Fractional a => Fractional (Point4 a) where
    recip = fmap recip
    {-# INLINE recip #-}
    (/) = liftA2 (/)
    {-# INLINE (/) #-}
    fromRational = pure . fromRational
    {-# INLINE fromRational #-}
