{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QualifiedDo #-}
module Ghengin.Component.Camera where

import Apecs
import Data.Word (Word32)

import Prelude.Linear (Ur(..), ($))
import Prelude hiding (($))

-- import Geomancy.Vulkan.Projection
import Geomancy.Mat4
import Geomancy.Vec3

import Ghengin.Utils (Epsilon(..))

import qualified Graphics.UI.GLFW as GLFW

import Ghengin.Vulkan.Renderer.GLFW.Window
import Ghengin.Vulkan.Renderer.Kernel -- We can't use Core yet to construct a renderer-agnostic battery pack because we need to be aware of GLFW in this module, for now.

import Ghengin.Component.Transform
import qualified Control.Functor.Linear as Linear
import qualified Control.Monad.IO.Class.Linear as Linear
import qualified Unsafe.Linear as Unsafe

import qualified Vulkan.Core10.FundamentalTypes (Extent2D(..))

data Camera = Camera !Projection !View deriving Show

data Projection = Orthogonal
                | Perspective { _fov  :: {-# UNPACK #-} !Float -- ^ Field of view in radians
                              , _near :: {-# UNPACK #-} !Float -- ^ Near plane
                              , _far  :: {-# UNPACK #-} !Float -- ^ Far plane
                              -- TODO: Cache matrix with laziness and trace to see if it's actually being cached
                              } deriving Show

-- A transform is required to calculate the view matrix (see 'makeView'). It's position determines the camera position and it's rotation determines the up in the case of LookAt and Direction
data View = ViewLookAt    { _target    :: {-# UNPACK #-} !Vec3 } -- ^ Ignore the camera's transform rotation (except for calculating up) and instead look at a target
          | ViewDirection { _direction :: {-# UNPACK #-} !Vec3 } -- ^ Ignore the camera's transform rotation (except for calculating up) and instead look in a direction
          | ViewTransform                                        -- ^ Use solely the transform associated to the camera for the view matrix
          deriving Show


instance Component Camera where
  type Storage Camera = Map Camera

makeView :: Mat4 -> View -> Mat4
makeView tr view =
  let pos = posFromMat4 tr
      up  = vec3 0 (-1) 0 -- TODO: Calculate up based on camera rotation
   in case view of
    ViewLookAt target -> makeView tr (ViewDirection (target - pos))
    ViewDirection dir ->
      let w@(WithVec3 wx wy wz) = normalize dir
          u@(WithVec3 ux uy uz) = normalize $ cross w up
          v@(WithVec3 vx vy vz) = cross w u
       in colMajor ux uy uz (-dot u pos)
                   vx vy vz (-dot v pos)
                   wx wy wz (-dot w pos)
                   0  0  0  1
    ViewTransform ->
      -- Compute the inverse of the transform matrix to get the view transform
      -- matrix, but do it directly because it's a homogenous rotation matrix
      withColMajor tr (\ux vx wx _px uy vy wy _py uz vz wz _pz _ _ _ _ ->
        let u = vec3 ux uy uz
            v = vec3 vx vy vz
            w = vec3 wx wy wz
         in colMajor ux uy uz (-dot u pos)
                     vx vy vz (-dot v pos)
                     wx wy wz (-dot w pos)
                     0  0  0  1)
      -- let WithVec3 rx ry rz = tr.rotation
      --     c3 = cos rz
      --     s3 = sin rz
      --     c2 = cos rx
      --     s2 = sin rx
      --     c1 = cos ry
      --     s1 = sin ry
      --     u@(WithVec3 ux uy uz) = vec3 (c1*c3 + s1*s2*s3) (c2*s3) (c1*s2*s3-c3*s1)
      --     v@(WithVec3 vx vy vz) = vec3 (c3*s1*s2-c1*s3) (c2*c3) (c1*c3*s2+s1*s3)
      --     w@(WithVec3 wx wy wz) = vec3 (c2*s1) (-s2) (c1*c2)
      --  in colMajor ux uy uz (-dot u pos)
      --              vx vy vz (-dot v pos)
      --              wx wy wz (-dot w pos)
      --              0  0  0  1
  

makeProjection :: Projection
               -> Renderer (Ur Mat4)
makeProjection = \case
  Perspective fovRad near far -> Linear.do
    Ur extent <- getRenderExtent
    Linear.pure $ Ur $ makePerspectiveProjection @Word32 fovRad near far extent.width extent.height
  Orthogonal -> Linear.liftSystemIO $ fail "TODO: Orthogonal projection not yet supported"


-- ^ Make a perspective camera using the renderer's width and height
makePerspectiveProjection :: Integral side
                      => Float -- ^ Field of View in radians
                      -> Float -- ^ Near plane
                      -> Float -- ^ Far plane
                      -> side  -- ^ Aspect width
                      -> side  -- ^ Aspect height
                      -> Mat4
makePerspectiveProjection fovRad near far (fromIntegral -> width) (fromIntegral -> height) =
  let tanHalfFovy = tan (fovRad/2)
   in colMajor
        (height/(width*tanHalfFovy)) 0 0 0
        0 (1/tanHalfFovy) 0 0
        0 0 (far/(far-near)) (-(far*near) / (far - near))
        0 0 1 0

  -- Camera $ unTransform $ perspective fovRad near far width height -- why is it negative??


updateFirstPersonCameraTransform :: Float -> Transform -> Renderer (Ur Transform)
updateFirstPersonCameraTransform dt tr = Linear.do
    Ur r <- ifPressed GLFW.Key'Right (vec3 0 1 0)    (vec3 0 0 0)
    Ur l <- ifPressed GLFW.Key'Left  (vec3 0 (-1) 0) (vec3 0 0 0)
    Ur u <- ifPressed GLFW.Key'Up    (vec3 1 0 0)    (vec3 0 0 0)
    Ur d <- ifPressed GLFW.Key'Down  (vec3 (-1) 0 0) (vec3 0 0 0)

    let rotateV = normalize (r + l + u + d)
    -- TODO: mod of y rotation with 2*pi

    let tr' = tr{rotation = if nearZero rotateV then tr.rotation else tr.rotation + rotateV ^* dt ^* lookSpeed} :: Transform

        WithVec3 rx ry rz = tr'.rotation

        forwardDir = vec3 (sin ry) 0 (cos ry)
        rightDir   = vec3 (cos ry) 0 (-sin ry)
        upDir      = vec3 0 (-1) 0

    Ur mf <- ifPressed GLFW.Key'W         (forwardDir)  (vec3 0 0 0)
    Ur mb <- ifPressed GLFW.Key'S         (-forwardDir) (vec3 0 0 0)
    Ur mr <- ifPressed GLFW.Key'D         (rightDir)    (vec3 0 0 0)
    Ur ml <- ifPressed GLFW.Key'A         (-rightDir)   (vec3 0 0 0)
    Ur mu <- ifPressed GLFW.Key'Space     (upDir)       (vec3 0 0 0)
    Ur md <- ifPressed GLFW.Key'LeftShift (-upDir)      (vec3 0 0 0)

    let moveDir = mf + mb + mr + ml + mu + md

        tr'' = tr'{position = if nearZero moveDir then tr'.position else tr'.position + moveDir ^* dt ^* moveSpeed} :: Transform

    Linear.pure $ Ur (tr'' :: Transform)
  where
    moveSpeed = 3
    lookSpeed = 2

    getKey :: GLFW.Key -> Renderer (Ur GLFW.KeyState)
    getKey k = Linear.do
      Ur unsafe_w <- renderer $ Unsafe.toLinear \renv -> Linear.pure (Ur renv._vulkanWindow._window, renv)
      Linear.liftSystemIOU $ GLFW.getKey unsafe_w k

    ifPressed :: GLFW.Key
              -> a -- ^ Then
              -> a -- ^ Else
              -> Renderer (Ur a) -- ^ Result
    ifPressed k t e = do
      getKey k Linear.>>= \case
        Ur GLFW.KeyState'Pressed -> Linear.pure (Ur t)
        Ur _ -> Linear.pure (Ur e)


-- * Utils

posFromMat4 :: Mat4 -> Vec3
posFromMat4 = flip withColMajor (\_ _ _ x _ _ _ y _ _ _ z _ _ _ _ -> vec3 x y z)
