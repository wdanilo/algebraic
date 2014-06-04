{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Math.Metric.Discrete where

import Math.Metric.Metric        (Metric(..), MetricCoord(..))
import Math.Coordinate.Cartesian (Cartesian(..), Point2(..))


data Discrete = Discrete deriving Show


--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance MetricCoord Discrete Cartesian where
    metricCoord _ = Cartesian

instance (Num a, Eq a) => Metric Discrete (Point2 a) a where
    distanceBase _ p1 p2 = if p1 == p2 then 0 else 1


