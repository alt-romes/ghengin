{-# LANGUAGE ViewPatterns #-}
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

import Ghengin.Vulkan
import qualified Vulkan

newtype Camera = Camera { projection :: Mat4 } deriving Show

instance Component Camera where
  type Storage Camera = Map Camera

instance (Monad m, HasField "cameras" w (Storage Camera)) => Has w m Camera where
  getStore = SystemT (asks (.cameras))

-- TODO: makeOrthographicCamera 
perspectiveCamera :: Float -- ^ Field of view in rads
                  -> Float -- ^ Near plane
                  -> Float -- ^ Far plane
                  -> Renderer Camera
perspectiveCamera fovRad near far = do
  extent <- getRenderExtent
  pure $ makePerspectiveCamera fovRad near far extent.width extent.height


-- ^ Make a perspective camera using the renderer's width and height
makePerspectiveCamera :: Integral side
                      => Float -- ^ Field of View in radians
                      -> Float -- ^ Near plane
                      -> Float -- ^ Far plane
                      -> side  -- ^ Aspect width
                      -> side  -- ^ Aspect height
                      -> Camera
makePerspectiveCamera fovRad near far (fromIntegral -> width) (fromIntegral -> height) =
  let tanHalfFovy = tan (fovRad/2)
   in Camera $ colMajor
        (height/(width*tanHalfFovy)) 0 0 0
        0 (1/tanHalfFovy) 0 0
        0 0 (far/(far-near)) (-(far*near) / (far - near))
        0 0 1 0

  -- Camera $ unTransform $ perspective fovRad near far width height -- why is it negative??


