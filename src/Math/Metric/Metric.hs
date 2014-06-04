{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Math.Metric.Metric where

import Math.Coordinate.Coordinate --(SpaceOf, CoordConversion, AutoConversion, convertCoord)

class Metric metric el result | metric el -> result where
    distanceBase :: metric -> el -> el -> result

class MetricCoord metric coord | metric -> coord where
    metricCoord  :: metric -> coord


--distance :: (MetricCoord metric coord
--            , CoordConversion AutoConversion coord a t
--            , CoordConversion AutoConversion coord b t
--            , space ~ SpaceOf a
--            , space ~ SpaceOf b
--            , Metric metric t) =>
--            metric -> space -> a -> b -> Double

distance metric space a b = distanceBase metric (convertCoord coord space a) (convertCoord coord space b)
    where coord = metricCoord metric


