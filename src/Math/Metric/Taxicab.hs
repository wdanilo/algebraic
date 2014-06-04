{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Math.Metric.Taxicab where

import Math.Metric.Metric        (Metric(..), MetricCoord(..))
import Math.Coordinate.Cartesian (Cartesian(..), Point2(..))


data Taxicab = Taxicab deriving Show


--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance MetricCoord Taxicab Cartesian where
    metricCoord _ = Cartesian

instance Num a => Metric Taxicab (Point2 a) a where
    distanceBase _ (Point2 x1 y1) (Point2 x2 y2) = x2-x1 + y2-y1


