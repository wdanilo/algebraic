{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Math.Metric.Euclidean where

import Math.Metric.Metric        (Metric(..), MetricCoord(..))
import Math.Coordinate.Cartesian (Cartesian(..), Point2(..))


data Euclidean = Euclidean deriving Show


--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance MetricCoord Euclidean Cartesian where
    metricCoord _ = Cartesian

instance Floating a => Metric Euclidean (Point2 a) a where
    distanceBase _ (Point2 x1 y1) (Point2 x2 y2) = sqrt $ (x2-x1)**2 + (y2-y1)**2