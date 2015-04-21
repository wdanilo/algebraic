{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Math.Coordinate.BiPolar where

import           Data.Typeable              (Typeable)
import           Control.Applicative
import           Data.Array.Accelerate
import           Data.Array.Accelerate.Smart
import           Data.Array.Accelerate.Product
import           Data.Array.Accelerate.Array.Sugar
import           Data.Complex
import qualified Data.Foldable as F

import qualified Math.Coordinate.Cartesian as Cartesian
import           Math.Coordinate.Cartesian    (Cartesian)
import           Math.Coordinate.Coordinate   (CoordConversion(..), ManualConversion(..), convertCoord)
import           Math.Space.Space             (Space2)

data BiPolar a = BiPolar !a deriving (Show)

data Point a = Point { a     :: !a
                     , sigma :: !a
                     , tau   :: !a
                     } deriving (Eq, Ord, Show, Read, Typeable)

toBiPolar = convertCoord . BiPolar

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Floating a => CoordConversion ManualConversion Cartesian space (Point a) (Cartesian.Point2 a) where
    convertCoordBase _ _ _ (Point a sigma tau) = Cartesian.Point2 x y where
        x = a * (sinh tau) / (cosh tau - cos sigma)
        y = a * (sin sigma) / (cosh tau - cos sigma)

instance (a~b) => CoordConversion ManualConversion (BiPolar b) space (Cartesian.Point2 a) (Point a) where
    convertCoordBase _ _ _ (Cartesian.Point2 x y) = undefined

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

type instance EltRepr (Point a)  = EltRepr (a, a, a)

instance Elt a => Elt (Point a) where
  eltType _ = eltType (undefined :: (a,a,a))
  toElt p = case toElt p of
     (x, y, z) -> Point x y z
  fromElt (Point x y z) = fromElt (x, y, z)

instance cst a => IsProduct cst (Point a) where
  type ProdRepr (Point a) = ProdRepr (a,a,a)
  fromProd cst (Point x y z) = fromProd cst (x,y,z)
  toProd cst t = case toProd cst t of
     (x, y, z) -> Point x y z
  prod cst _ = prod cst (undefined :: (a,a,a))

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Point a) where
  type Plain (Point a) = Point (Plain a)
  lift (Point x y z) = Exp $ Tuple $ NilTup `SnocTup` lift x `SnocTup` lift y `SnocTup` lift z

instance (Elt a, e ~ Exp a) => Unlift Exp (Point e) where
  unlift t = Point (Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t)
                (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
                (Exp $ ZeroTupIdx `Prj` t)
