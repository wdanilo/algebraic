module Math.Metric (
	module X
) where

import Math.Space.MetricSpace as X
import Math.Metric.Metric     as X hiding (distance)
import Math.Metric.Chebyshev  as X
import Math.Metric.Discrete   as X
import Math.Metric.Euclidean  as X
import Math.Metric.Minkowski  as X
import Math.Metric.Taxicab    as X
