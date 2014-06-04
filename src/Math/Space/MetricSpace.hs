{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Math.Space.MetricSpace where

import qualified Math.Metric.Metric as Metric
import           Math.Metric.Metric         (Metric, MetricCoord)
import           Math.Coordinate.Coordinate --(SpaceOf, CoordConversion, AutoConversion, convertCoord)


data MetricSpace metric space = MetricSpace { metric :: metric
                                            , space  :: space 
                                            } deriving (Show)


--distance :: (MetricCoord metric coord
--            , CoordConversion AutoConversion coord a t
--            , CoordConversion AutoConversion coord b t
--            , space ~ SpaceOf a
--            , space ~ SpaceOf b
--            , Metric metric t) =>
--            (MetricSpace metric space) -> a -> b -> Double
distance (MetricSpace metric space) = Metric.distance metric space



--class Distance2 a b where
--    distance2 :: a -> b


--data X a = X a

--instance (MetricCoord metric coord
--            , CoordConversion AutoConversion coord a t
--            , CoordConversion AutoConversion coord b t
--            , space ~ SpaceOf a
--            , space ~ SpaceOf b
--            , Metric metric t
--            , out ~ Double) =>
--        Distance2 (X metric) (space -> a -> b -> out) where
--    distance2 (X metric) = Metric.distance metric

--instance (MetricCoord metric coord
--            , CoordConversion AutoConversion coord a t
--            , CoordConversion AutoConversion coord b t
--            , space ~ SpaceOf a
--            , space ~ SpaceOf b
--            , Metric metric t
--            , out ~ Double) =>
--        Distance2 (MetricSpace metric space) (a -> b -> out) where
--    distance2 = distance