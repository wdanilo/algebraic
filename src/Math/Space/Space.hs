---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
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

module Math.Space.Space where

import           Data.Array.Accelerate
import           Data.Array.Accelerate.Smart
import           Data.Array.Accelerate.Product
import           Data.Array.Accelerate.Array.Sugar
import           Data.Complex
import qualified Data.Foldable as F
import           Data.Typeable

import Control.Applicative

--data Space2 = Space2 !Double !Double deriving (Show)

class Space2 space where
	width  :: space a -> a
	height :: space a -> a

class Space2 space => Space3 space where
	depth :: space a -> a

data Grid a = Grid !a !a deriving (Eq, Ord, Show, Read,Typeable)

instance Space2 Grid where
	width  (Grid x _) = x
	height (Grid _ x) = x

--------------------------------------------------------------------------------
-- Grid
--------------------------------------------------------------------------------
instance Functor Grid where
    fmap f (Grid a b) = Grid (f a) (f b)

instance Applicative Grid where
    pure a = Grid a a
    {-# INLINE pure #-}
    Grid a b <*> Grid d e = Grid (a d) (b e)
    {-# INLINE (<*>) #-}

instance Num a => Num (Grid a) where
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

instance Fractional a => Fractional (Grid a) where
    recip = fmap recip
    {-# INLINE recip #-}
    (/) = liftA2 (/)
    {-# INLINE (/) #-}
    fromRational = pure . fromRational
    {-# INLINE fromRational #-}

type instance EltRepr (Grid a)  = EltRepr (a, a)

instance Elt a => Elt (Grid a) where
  eltType _ = eltType (undefined :: (a,a))
  toElt p = case toElt p of
     (x, y) -> Grid x y
  fromElt (Grid x y) = fromElt (x, y)

instance cst a => IsProduct cst (Grid a) where
  type ProdRepr (Grid a) = ProdRepr (a,a)
  fromProd cst (Grid x y) = fromProd cst (x,y)
  toProd cst t = case toProd cst t of
     (x, y) -> Grid x y
  prod cst _ = prod cst (undefined :: (a,a))

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Grid a) where
  type Plain (Grid a) = Grid (Plain a)
  lift (Grid x y) = Exp $ Tuple $ NilTup `SnocTup` lift x `SnocTup` lift y

instance (Elt a, e ~ Exp a) => Unlift Exp (Grid e) where
  unlift t = Grid (Exp $ SuccTupIdx ZeroTupIdx `Prj` t)
                (Exp $ ZeroTupIdx `Prj` t)
