module DataStructures where

import Data.Map

instance Num Vec3 where
  negate (Vec3 x1 x2 x3) = Vec3 (-x1) (-x2) (-x3)
  (+) (Vec3 x1 x2 x3) (Vec3 y1 y2 y3) = Vec3 (x1 + y1) (x2 + y2) (x3 + y3)
  (*) (Vec3 x1 x2 x3) (Vec3 y1 y2 y3) = Vec3 (x1 * y1) (x2 * y2) (x3 * y3)
  abs (Vec3 x1 x2 x3) = Vec3 (abs x1) (abs x2) (abs x3)
  signum (Vec3 x1 x2 x3) = Vec3 (signum x1) (signum x2) (signum x3)
  fromInteger x1 = Vec3 (fromInteger x1) (fromInteger x1) (fromInteger x1)


data Vec3 = Vec3 { x :: Double
                 , y :: Double
                 , z :: Double } deriving (Show, Eq)

type Point3d = Vec3 -- Named Vec3 for 3d points in space
type Dir = Vec3 -- Always normalized Vec3

data Basis = Basis { u :: Vec3
                   , v :: Vec3
                   , w :: Vec3 } deriving (Show, Eq)

-- data Ray = Ray { o :: Point3d
--                , dir :: Dir } deriving (Show, Eq)

data Camera = Camera { e :: Point3d
                     , dir :: Dir
                     , dist :: Double
                     , resx :: Double
                     , resy :: Double
                     , fovx :: Double
                     , fovy :: Double
                     , basis :: Basis } deriving (Show, Eq)

type ViewPlane = (Int, Int)

data Color = Color { r :: Double
                   , g :: Double
                   , b :: Double } deriving (Show, Eq)

data Object = Sphere Point3d Double
            | Cube Point3d Double
            | RectPrism Point3d Double Double Double -- l,w,h
  deriving (Show, Eq)

type Bound = (Double, Double, Double)
-- xmin, ymin, ..

data Light = Light { shape :: Object
                   , loc :: Point3d } deriving (Show, Eq)

data World = World { objects :: [Object]
                   , light :: Object
                   , bounds :: Vec3 } deriving (Show, Eq)

type Point2d = Int -> Int



type BitMap = Map Point2d Color
