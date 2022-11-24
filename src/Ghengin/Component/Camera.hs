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

import Geomancy.Vulkan.Projection
import Geomancy.Mat4
import Geomancy.Vec3

import Ghengin.Vulkan
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
      up  = vec3 0 1 0 -- TODO: Calculate up based on camera rotation
   in case view of
    ViewLookAt target -> makeView tr (ViewDirection (target - pos))
    ViewDirection dir ->
      let w@(WithVec3 wx wy wz) = normalize dir
          u@(WithVec3 ux uy uz) = normalize $ cross dir up
          v@(WithVec3 vx vy vz) = cross w u
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


