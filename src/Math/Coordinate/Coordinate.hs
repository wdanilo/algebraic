{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module Math.Coordinate.Coordinate where

import GHC.Exts

-- convType is used to prevent context reduction stack overflow when combining with
-- automatic coordinate conversion
class CoordConversion convType sys space c1 c2 | sys c1 -> c2 where
    convertCoordBase :: convType -> sys -> space -> c1 -> c2


data ManualConversion = ManualConversion deriving Show
data AutoConversion = AutoConversion deriving Show

--convertCoord :: (CoordConversion AutoConversion sys a b, SpaceOf a space) => 
--             sys -> space -> a -> b
convertCoord = convertCoordBase AutoConversion