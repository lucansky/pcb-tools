{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Data.Gerber
import Text.Pretty.Simple (pPrint)
import qualified Data.ByteString as BS

import Data.Geometry.Polygon
import qualified Data.CircularSeq as C
import Data.Geometry.Point
import Data.Ext
import Data.Scientific hiding (scientific)
import Algorithms.Geometry.DelaunayTriangulation.DivideAndConqueror
import Data.Geometry.Polygon.Convex
import qualified Data.List.NonEmpty as NonEmpty

main :: IO ()
main = exampleMain

exampleMain = do
  BS.readFile "example/scale_short.gbr" >>= parseGerber >>= pPrint
  return ()

p1 :: SimplePolygon () Rational
p1 = SimplePolygon . C.fromList . map ext $ [ point2 0 0
                                            , point2 10 0
                                            , point2 10 10
                                            , point2 5 15
                                            , point2 1 11
                                            ]

p2 :: SimplePolygon () Rational
p2 = SimplePolygon . C.fromList . map ext $ [ point2 0 0
                                             , point2 0 2
                                             , point2 2 2
                                             , point2 2 0
                                            ]

p3 :: SimplePolygon () Rational
p3 = SimplePolygon . C.fromList . map ext $ [ point2 4 4
                                             , point2 4 5
                                             , point2 5 5
                                             , point2 5 4
                                            ]

nes1 :: NonEmpty.NonEmpty (Point 2 Rational :+ ())
nes1 = NonEmpty.fromList . map ext $ [point2 0 0, point2 0 2, point2 2 2, point2 2 0]

test1 :: Num r => ConvexPolygon () r
test1 = ConvexPolygon . fromPoints . map ext . reverse $ [origin, point2 1 4, point2 5 6, point2 10 3]

test2 :: Num r => ConvexPolygon () r
test2 = ConvexPolygon . fromPoints . map ext . reverse $ [point2 (-1) (-1), point2 (-1) 1, point2 1 1, point2 1 (-1)]

--graphicsPlayground =
--  do
