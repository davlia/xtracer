{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, RecordWildCards #-}


{-
forward backward x
left right y
up down z

-}

module Main where

import Data.List hiding (intersect)

import DataStructures
import Geometry

import Codec.Picture

path :: String
path = "./test.png"

black :: PixelRGBA8
black = PixelRGBA8 0 0 0 0

gray :: PixelRGBA8
gray = PixelRGBA8 127 127 127 127

white :: PixelRGBA8
white = PixelRGBA8 255 255 255 255

sphere1 :: Object
sphere1 = Sphere (Vec3 250 400 300) 50.0

sphere2 :: Object
sphere2 = Sphere (Vec3 250 600 300) 50.0

defLight :: Object
defLight = Sphere (Vec3 250 400 0) 100.0

world :: World
world = World [sphere1, sphere2] defLight (Vec3 500 500 500)

defBasis :: Basis
defBasis = Basis (Vec3 0 0 1) (Vec3 0 1 0) (Vec3 1 0 0)

defLoc :: Point3d
defLoc = Vec3 (-50) 400 300

defDir :: Dir
defDir = Vec3 1 0 0

camera :: Camera
camera = Camera defLoc defDir 10 800 600 130 130 defBasis

-- Gets the direction of the ray wrt the world coordinates
genRay :: Camera -> Int -> Int -> Dir
genRay Camera{..} px py = normalize $ scale dist dir + scale z (v basis) + scale y (u basis)
  where
    ratio = resx / resy
    vx = ratio * dist * tan (fovx * pi / 360)
    vy = dist * tan (fovy * pi / 360)
    aspx = 2 * vx / resx
    aspy = 2 * vy / resy
    z = aspx * (fromIntegral px + 0.5) - vx
    y = aspy * (fromIntegral py + 0.5) - vy

comparator :: Ord a => Maybe (t, a) -> Maybe (t1, a) -> Ordering
comparator (Just (_, x2)) (Just (_,y2)) = compare x2 y2

getIntersection :: Point3d -> Dir -> [Object] -> Maybe (Object, Double)
getIntersection e dir o =
  case filter (Nothing /=) $ map (intersect e dir) o of
    [] -> Nothing
    xs -> minimumBy comparator xs

intersect :: Point3d -> Dir -> Object -> Maybe (Object, Double)
intersect e dir o@(Sphere s r) =
  if det < 0
  then Nothing
  else let dt = sqrt det in Just (o, min (tm + dt) (tm - dt))
  where
    etos = s - e
    tm = dir `dot` etos
    det = (tm^2) - (etos `dot` etos) + (r^2)
intersect _ _ _ = Nothing

getShading :: World -> Point3d -> PixelRGBA8
getShading (World objs (Sphere l _) _) o =
  case getIntersection o lightdir objs of
    Nothing -> white
    Just _ -> gray
  where
    lightdir = normalize (l - o)

-- getPixelColor' :: Camera -> World -> Int -> Int -> IO PixelRGBA8
-- getPixelColor' Camera{..} w@World{..} x y = do
--   let ints = getIntersection e d objects
--   print ints
--   case ints of
--     Nothing -> return black
--     Just (o, t) -> do
--       print (normal o t)
--       print (hit t - (oloc o))
--       return $ genColor (normal o t)  --getShading w hit t
--   where
--     d = genRay camera x y
--     hit t = e + scale t dir
--     oloc (Sphere p r) = p
--     normal o t = normalize (hit t - (oloc o))
--     genColor (Vec3 r g b) = PixelRGBA8 (floor . abs $ r*255) (floor . abs $  g*255) (floor . abs $  b*255) 255

getPixelColor :: Camera -> World -> Int -> Int -> PixelRGBA8
getPixelColor Camera{..} w@World{..} x y = do
  case getIntersection e d objects of
    Nothing -> white
    Just (o, t) -> getShading w (hit t)
  where
    d = genRay camera x y
    hit t = e + scale t d
    oloc (Sphere p _) = p
    normal o t = normalize (hit t - (oloc o))
    genColor (Vec3 r g b) = PixelRGBA8 (floor . abs $ r*255) (floor . abs $  g*255) (floor . abs $  b*255) 255

main :: IO ()
main = do
  writePng path $ generateImage pixelRenderer (floor $ resx camera) (floor $ resy camera)
    where pixelRenderer = getPixelColor camera world
