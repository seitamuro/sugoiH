module Shapes
( Point(..), -- (..) exports all value constructors for Point
  Shape(..), -- (..) exports all value constructors for Shape
  area,
) where

import qualified Geometry.Sphere as Sphere

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point
  deriving (Show)

area :: Shape -> Float
area (Circle _ r) = Sphere.area r
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)