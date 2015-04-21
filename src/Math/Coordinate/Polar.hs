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

module Math.Coordinate.Polar where

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

data Polar      = Polar deriving (Show)

data Point2 a = Point2 { r   :: !a
                       , phi :: !a
                       } deriving (Eq, Ord, Show, Read, Typeable)

toPolar = convertCoord Polar

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Floating a => CoordConversion ManualConversion Cartesian space (Point2 a) (Cartesian.Point2 a) where
    convertCoordBase _ _ _ (Point2 r phi) = Cartesian.Point2 (r * cos phi) (r * sin phi)

instance RealFloat a => CoordConversion ManualConversion Polar space (Cartesian.Point2 a) (Point2 a) where
    convertCoordBase _ _ _ (Cartesian.Point2 x y) = Point2 r phi
        where r   = sqrt $ (x*x) + (y*y)
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

type instance EltRepr (Point2 a)  = EltRepr (a, a)

instance Elt a => Elt (Point2 a) where
  eltType _ = eltType (undefined :: (a,a))
  toElt p = case toElt p of
     (x, y) -> Point2 x y
  fromElt (Point2 x y) = fromElt (x, y)

instance cst a => IsProduct cst (Point2 a) where
  type ProdRepr (Point2 a) = ProdRepr (a,a)
  fromProd cst (Point2 x y) = fromProd cst (x,y)
  toProd cst t = case toProd cst t of
     (x, y) -> Point2 x y
  prod cst _ = prod cst (undefined :: (a,a))

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Point2 a) where
  type Plain (Point2 a) = Point2 (Plain a)
  lift (Point2 x y) = Exp $ Tuple $ NilTup `SnocTup` lift x `SnocTup` lift y

instance (Elt a, e ~ Exp a) => Unlift Exp (Point2 e) where
  unlift t = Point2 (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
                (Exp $ ZeroTupIdx `Prj` t)
