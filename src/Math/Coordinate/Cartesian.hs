{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Math.Coordinate.Cartesian where

import           Control.Applicative
import           Data.Array.Accelerate
import           Data.Array.Accelerate.Smart
import           Data.Array.Accelerate.Tuple
import           Data.Array.Accelerate.Array.Sugar
import           Data.Complex
import qualified Data.Foldable as F
import           Data.Typeable
import           GHC.Generics (Generic)

import Math.Coordinate.Coordinate (CoordConversion(..), ManualConversion(..), AutoConversion(..), convertCoord)
import Math.Space.Space           (Space2)

data Cartesian  = Cartesian deriving (Show)

data Point1 a   = Point1 !a deriving (Eq, Ord, Show, Read, Typeable, Generic)
data Point2 a   = Point2 !a !a deriving (Eq, Ord, Show, Read, Typeable, Generic)
data Point3 a   = Point3 !a !a !a deriving (Eq, Ord, Show, Read, Typeable, Generic)
data Point4 a   = Point4 !a !a !a !a deriving (Eq, Ord, Show, Read, Typeable, Generic)

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

type instance EltRepr (Point4 a)  = EltRepr (a, a, a, a)
type instance EltRepr' (Point4 a) = EltRepr' (a, a, a, a)

instance Elt a => Elt (Point4 a) where
  eltType _ = eltType (undefined :: (a,a,a,a))
  toElt p = case toElt p of
     (x, y, z, w) -> Point4 x y z w
  fromElt (Point4 x y z w) = fromElt (x, y, z, w)

  eltType' _ = eltType' (undefined :: (a,a,a,a))
  toElt' p = case toElt' p of
     (x, y, z, w) -> Point4 x y z w
  fromElt' (Point4 x y z w) = fromElt' (x, y, z, w)

instance IsTuple (Point4 a) where
  type TupleRepr (Point4 a) = TupleRepr (a,a,a,a)
  fromTuple (Point4 x y z w) = fromTuple (x,y,z,w)
  toTuple t = case toTuple t of
     (x, y, z, w) -> Point4 x y z w

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Point4 a) where
  type Plain (Point4 a) = Point4 (Plain a)
  --  lift = Exp . Tuple . F.foldl SnocTup NilTup
  lift (Point4 x y z w) = Exp $ Tuple $ NilTup `SnocTup`
                      lift x `SnocTup`
                      lift y `SnocTup`
                      lift z `SnocTup`
                      lift w

instance (Elt a, e ~ Exp a) => Unlift Exp (Point4 e) where
  unlift t = Point4 (Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` t)
                (Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t)
                (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
                (Exp $ ZeroTupIdx `Prj` t)
