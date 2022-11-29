{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-} -- instance Has w m Mesh
module Ghengin.Component.Camera where

import GHC.Records
import Apecs

-- import Geomancy.Vulkan.Projection
import Geomancy.Mat4
import Geomancy.Vec3

import qualified Graphics.UI.GLFW as GLFW

import Ghengin.Vulkan.GLFW.Window
import Ghengin.Vulkan
import Ghengin.Utils
import qualified Vulkan

import Ghengin.Component.Transform

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

instance (Monad m, HasField "cameras" w (Storage Camera)) => Has w m Camera where
  getStore = SystemT (asks (.cameras))


makeView :: Transform -> View -> Mat4
makeView tr view =
  let pos = tr.position
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
      let WithVec3 rx ry rz = tr.rotation
          c3 = cos rz
          s3 = sin rz
          c2 = cos rx
          s2 = sin rx
          c1 = cos ry
          s1 = sin ry
          u@(WithVec3 ux uy uz) = vec3 (c1*c3 + s1*s2*s3) (c2*s3) (c1*s2*s3-c3*s1)
          v@(WithVec3 vx vy vz) = vec3 (c3*s1*s2-c1*s3) (c2*c3) (c1*c3*s2+s1*s3)
          w@(WithVec3 wx wy wz) = vec3 (c2*s1) (-s2) (c1*c2)
       in colMajor ux uy uz (-dot u pos)
                   vx vy vz (-dot v pos)
                   wx wy wz (-dot w pos)
                   0  0  0  1
  

makeProjection :: Projection
               -> Renderer Mat4
makeProjection = \case
  Perspective fovRad near far -> do
    extent <- getRenderExtent
    pure $ makePerspectiveProjection fovRad near far extent.width extent.height
  Orthogonal -> liftIO $ fail "TODO: Orthogonal projection not yet supported"


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


updateFirstPersonCameraTransform :: Float -> Transform -> Renderer Transform
updateFirstPersonCameraTransform dt tr = do
    r <- ifPressed GLFW.Key'Right (pure $ vec3 0 1 0) (pure $ vec3 0 0 0)
    l <- ifPressed GLFW.Key'Left (pure $ vec3 0 (-1) 0) (pure $ vec3 0 0 0)
    u <- ifPressed GLFW.Key'Up   (pure $ vec3 1 0 0) (pure $ vec3 0 0 0)
    d <- ifPressed GLFW.Key'Down (pure $ vec3 (-1) 0 0) (pure $ vec3 0 0 0)

    let rotateV = normalize (r + l + u + d)
    -- TODO: mod of y rotation with 2*pi

    let tr' = tr{rotation = if nearZero rotateV then tr.rotation else tr.rotation + rotateV ^* dt ^* lookSpeed} :: Transform

        WithVec3 rx ry rz = tr'.rotation

        forwardDir = vec3 (sin ry) 0 (cos ry)
        rightDir   = vec3 (cos ry) 0 (-sin ry)
        upDir      = vec3 0 (-1) 0

    mf <- ifPressed GLFW.Key'W (pure forwardDir) (pure $ vec3 0 0 0)
    mb <- ifPressed GLFW.Key'S (pure (-forwardDir)) (pure $ vec3 0 0 0)
    mr <- ifPressed GLFW.Key'D (pure rightDir) (pure $ vec3 0 0 0)
    ml <- ifPressed GLFW.Key'A (pure (-rightDir)) (pure $ vec3 0 0 0)
    mu <- ifPressed GLFW.Key'Space (pure upDir) (pure $ vec3 0 0 0)
    md <- ifPressed GLFW.Key'LeftShift (pure (-upDir)) (pure $ vec3 0 0 0)

    let moveDir = mf + mb + mr + ml + mu + md

        tr'' = tr'{position = if nearZero moveDir then tr'.position else tr'.position + moveDir ^* dt ^* moveSpeed} :: Transform

    pure (tr'' :: Transform)
  where
    moveSpeed = 3
    lookSpeed = 2

    getKey :: GLFW.Key -> Renderer GLFW.KeyState
    getKey k = do
      w <- asks (._vulkanWindow._window)
      liftIO $ GLFW.getKey w k

    ifPressed :: GLFW.Key
              -> Renderer a -- ^ Then
              -> Renderer a -- ^ Else
              -> Renderer a -- ^ Result
    ifPressed k t e = do
      getKey k >>= \case
        GLFW.KeyState'Pressed -> t
        _ -> e
