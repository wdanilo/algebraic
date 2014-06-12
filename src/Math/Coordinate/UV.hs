{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Math.Coordinate.UV where

import           Data.Typeable                 (Typeable)
import           Control.Applicative
import           Data.Array.Accelerate
import           Data.Array.Accelerate.Smart
import           Data.Array.Accelerate.Tuple
import           Data.Array.Accelerate.Array.Sugar
import           Data.Complex
import qualified Data.Foldable as F

import qualified Math.Coordinate.Cartesian  as Cartesian
import           Math.Coordinate.Cartesian     (Cartesian)
import           Math.Coordinate.Coordinate    (CoordConversion(..), ManualConversion(..), convertCoord)
import qualified Math.Space.Space           as Space
import           Math.Space.Space              (Space2)

data UV         = UV deriving (Show)

data Point1 a = Point1 !a deriving (Eq, Ord, Show, Read, Typeable)
data Point2 a = Point2 !a !a deriving (Eq, Ord, Show, Read, Typeable)
data Point3 a = Point3 !a !a !a deriving (Eq, Ord, Show, Read, Typeable)

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

instance UVCoord1 Point1 where u (Point1 u) = u

instance UVCoord1 Point2 where u (Point2 u _) = u
instance UVCoord2 Point2 where v (Point2 _ v) = v

instance UVCoord1 Point3 where u (Point3 u _ _) = u
instance UVCoord2 Point3 where v (Point3 _ v _) = v
instance UVCoord3 Point3 where w (Point3 _ _ w) = w


instance (Space2 space, Num a, a~b) => CoordConversion ManualConversion Cartesian (space b) (Point2 a) (Cartesian.Point2 a) where
    convertCoordBase _ _ space (Point2 u v) = Cartesian.Point2 (u*w) (v*h)
        where w = Space.width  space
              h = Space.height space

instance (Space2 space, Fractional a, a~b) => CoordConversion ManualConversion UV (space b) (Cartesian.Point2 a) (Point2 a) where
    convertCoordBase _ sys space (Cartesian.Point2 x y) = Point2 (x/w) (y/h)
        where w = Space.width  space
              h = Space.height space

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

type instance EltRepr (Point1 a) = EltRepr a
type instance EltRepr' (Point1 a) = EltRepr' a

instance Elt a => Elt (Point1 a) where
  eltType _ = eltType (undefined :: a)
  toElt = Point1 . toElt
  fromElt (Point1 a) = fromElt a

  eltType' _ = eltType' (undefined :: a)
  toElt' = Point1 . toElt'
  fromElt' (Point1 a) = fromElt' a

instance IsTuple (Point1 a) where
  type TupleRepr (Point1 a) = ((), a)
  fromTuple (Point1 x) = ((), x)
  toTuple ((), x) = Point1 x

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Point1 a) where
  type Plain (Point1 a) = Point1 (Plain a)
  lift (Point1 x) = Exp . Tuple $ NilTup `SnocTup` lift x

instance (Elt a, e ~ Exp a) => Unlift Exp (Point1 e) where
  unlift t = Point1 $ Exp $ ZeroTupIdx `Prj` t

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

type instance EltRepr (Point2 a)  = EltRepr (a, a)
type instance EltRepr' (Point2 a) = EltRepr' (a, a)

instance Elt a => Elt (Point2 a) where
  eltType _ = eltType (undefined :: (a,a))
  toElt p = case toElt p of
     (x, y) -> Point2 x y
  fromElt (Point2 x y) = fromElt (x, y)

  eltType' _ = eltType' (undefined :: (a,a))
  toElt' p = case toElt' p of
     (x, y) -> Point2 x y
  fromElt' (Point2 x y) = fromElt' (x, y)

instance IsTuple (Point2 a) where
  type TupleRepr (Point2 a) = TupleRepr (a,a)
  fromTuple (Point2 x y) = fromTuple (x,y)
  toTuple t = case toTuple t of
     (x, y) -> Point2 x y

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Point2 a) where
  type Plain (Point2 a) = Point2 (Plain a)
  --lift = Exp . Tuple . F.foldl SnocTup NilTup
  lift (Point2 x y) = Exp $ Tuple $ NilTup `SnocTup` lift x `SnocTup` lift y

instance (Elt a, e ~ Exp a) => Unlift Exp (Point2 e) where
  unlift t = Point2 (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
                (Exp $ ZeroTupIdx `Prj` t)

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

type instance EltRepr (Point3 a)  = EltRepr (a, a, a)
type instance EltRepr' (Point3 a) = EltRepr' (a, a, a)

instance Elt a => Elt (Point3 a) where
  eltType _ = eltType (undefined :: (a,a,a))
  toElt p = case toElt p of
     (x, y, z) -> Point3 x y z
  fromElt (Point3 x y z) = fromElt (x, y, z)

  eltType' _ = eltType' (undefined :: (a,a,a))
  toElt' p = case toElt' p of
     (x, y, z) -> Point3 x y z
  fromElt' (Point3 x y z) = fromElt' (x, y, z)

instance IsTuple (Point3 a) where
  type TupleRepr (Point3 a) = TupleRepr (a,a,a)
  fromTuple (Point3 x y z) = fromTuple (x,y,z)
  toTuple t = case toTuple t of
     (x, y, z) -> Point3 x y z

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Point3 a) where
  type Plain (Point3 a) = Point3 (Plain a)
  --lift = Exp . Tuple . F.foldl SnocTup NilTup
  lift (Point3 x y z) = Exp $ Tuple $ NilTup `SnocTup` lift x `SnocTup` lift y `SnocTup` lift z

instance (Elt a, e ~ Exp a) => Unlift Exp (Point3 e) where
  unlift t = Point3 (Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t)
                (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
                (Exp $ ZeroTupIdx `Prj` t)
