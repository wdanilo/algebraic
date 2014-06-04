{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Math.Metric.Minkowski where

import Math.Metric.Metric        (Metric(..), MetricCoord(..))
import Math.Coordinate.Cartesian (Cartesian(..), Point2(..))


data Minkowski a = Minkowski a deriving Show


--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance MetricCoord (Minkowski a) Cartesian where
    metricCoord _ = Cartesian

instance Floating a => Metric (Minkowski a) (Point2 a) a where
    distanceBase (Minkowski p) (Point2 x1 y1) (Point2 x2 y2) = ((abs $ x1-x2)**p + (abs $ y1-y2)**p)**(1/p)