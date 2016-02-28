module Geometry where

import DataStructures

dot :: Vec3 -> Vec3 -> Double
dot (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

scale :: Double -> Vec3 -> Vec3
scale d (Vec3 x1 x2 x3) = Vec3 (d * x1) (d * x2) (d * x3)

normalize :: Vec3 -> Dir
normalize (Vec3 x1 x2 x3) = Vec3 (x1 / n) (x2 / n) (x3 / n)
  where n = sqrt(x1^2 + x2^2 + x3^2)
