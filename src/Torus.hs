{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Torus where

import Data.Foldable

data Torus = Torus { holeRadius :: Float
                   , tubeRadius :: Float}
                   deriving (Show, Eq)

class Vector s a | a -> s where
  (^+) :: a -> a -> a
  (^*) :: s -> a -> a

data Vector2D a = Vector2D !a !a
  deriving (Show, Eq)
data Vector3D a = Vector3D !a !a !a
  deriving (Show, Eq)

instance Num a => Vector a (Vector2D a) where
  (Vector2D a b) ^+ (Vector2D x y) = Vector2D (a+x) (b+y)
  s ^* (Vector2D x y) = Vector2D (s*x) (s*y)

instance Num a => Vector a (Vector3D a) where
  (Vector3D a b c) ^+ (Vector3D x y z) = Vector3D (a+x) (b+y) (c+z)
  s ^* (Vector3D x y z) = Vector3D (s*x) (s*y) (s*z)

instance Foldable Vector2D where
  foldr f x (Vector2D a b) = f a $ f b x

instance Foldable Vector3D where
  foldr f x (Vector3D a b c) = f a $ f b $ f c x

toTorusCoordinates :: Torus -> Vector2D Float -> Vector3D Float
toTorusCoordinates t (Vector2D x' y') =
    holeRadius t ^* direction
 ^+ ( (tubeRadius t * cos y) ^* direction)
 ^+ ( (tubeRadius t * sin y) ^* Vector3D 0 1 0)
  where
    x = 2*pi*x'
    y = 2*pi*y'
    direction = Vector3D (cos x) 0 (sin x)
