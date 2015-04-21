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

module Math.Coordinate.Parabolic where

import           Data.Typeable                 (Typeable)
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

type instance EltRepr (Point a)  = EltRepr (a, a)

instance Elt a => Elt (Point a) where
  eltType _ = eltType (undefined :: (a,a))
  toElt p = case toElt p of
     (x, y) -> Point x y
  fromElt (Point x y) = fromElt (x, y)

instance cst a => IsProduct cst (Point a) where
  type ProdRepr (Point a) = ProdRepr (a,a)
  fromProd cst (Point x y) = fromProd cst (x,y)
  toProd cst t = case toProd cst t of
     (x, y) -> Point x y
  prod cst _ = prod cst (undefined :: (a,a))

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Point a) where
  type Plain (Point a) = Point (Plain a)
  lift (Point x y) = Exp $ Tuple $ NilTup `SnocTup` lift x `SnocTup` lift y

instance (Elt a, e ~ Exp a) => Unlift Exp (Point e) where
  unlift t = Point (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
                (Exp $ ZeroTupIdx `Prj` t)
