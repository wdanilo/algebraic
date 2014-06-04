{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Math.Metric.Chebyshev where

import Math.Metric.Metric        (Metric(..), MetricCoord(..))
import Math.Coordinate.Cartesian (Cartesian(..), Point2(..))


data Chebyshev = Chebyshev deriving Show


--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance MetricCoord Chebyshev Cartesian where
    metricCoord _ = Cartesian

instance (Num a, Ord a) => Metric Chebyshev (Point2 a) a where
    distanceBase _ (Point2 x1 y1) (Point2 x2 y2) = max (abs $ x2-x1) (abs $ y2-y1)


