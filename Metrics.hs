{-# LANGUAGE NoMonomorphismRestriction #-}

import qualified Math.Coordinate.Cartesian as Cartesian
--import qualified Math.Coordinate.Polar     as Polar
--import qualified Math.Coordinate.UV        as UV
import Math.Space.Space
import           Math.Coordinate

import Math.Metric

import Math.Coordinate.Coordinate 


main = do
    let space = Grid 51 60
    --let space = Grid (51::Float) (60::Float)
        mspace = MetricSpace Euclidean space
        
        --p1    = Cartesian.Point2 (-1::Float) (-10::Float)
        --p2    = Cartesian.Point2 (8::Float) (4::Float)

        p1    = Cartesian.Point2 (-1) (-10)
        p2    = Cartesian.Point2 8 4

        pp1   = toPolar space p1
        u1    = toUV space p1

    print pp1
    print $ distance mspace p1 p2
    print $ toLogPolar space pp1
    print $ distance mspace u1 p2
    print $ toElliptic 1 space p1
    print $ toCartesian space $ toElliptic 1 space p1
    print "end"



