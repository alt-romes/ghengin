{-# OPTIONS_GHC -Wno-orphans #-} -- Transform instances
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-} -- HasField Has Transform
module Ghengin.Component.Transform
  ( Transform(..)
  , noTransform
  , applyTransform
  ) where

import GHC.Records

import Geomancy hiding (Transform)
import Geomancy.Mat4
import Geomancy.Transform hiding (Transform)
import qualified Vulkan as Vk
import Apecs

import Ghengin.Vulkan.Command
import Ghengin.Vulkan.Pipeline

data Transform = Transform { position :: {-# UNPACK #-} !Vec3
                           , scale    :: {-# UNPACK #-} !Vec3
                           , rotation :: {-# UNPACK #-} !Vec3
                           }

noTransform :: Transform
noTransform = Transform (vec3 0 0 0) (vec3 1 1 1) (vec3 0 0 0)

-- TODO: Abstract data type that caches the resulting transformation, and
-- provides an update operation that updates always updates the cache

instance Component Transform where
  type Storage Transform = Map Transform

-- TODO: Instructions on having a World record with "transforms"
instance (Monad m, HasField "transforms" w (Storage Transform)) => Has w m Transform where
  getStore = SystemT (asks (.transforms))

applyTransform :: VulkanPipeline -> Transform -> RenderPassCmd
applyTransform pipeline tr = do
  pushConstants pipeline._pipelineLayout Vk.SHADER_STAGE_VERTEX_BIT (makeTransform tr)

-- Make a Matrix 4 by applying translate * R(y) * R(x) * R(z) * scale transformation
makeTransform :: Transform -> Mat4
makeTransform tr =
  let 
      WithVec3 x y z    = tr.position
      WithVec3 sx sy sz = tr.scale
      WithVec3 rx ry rz = tr.rotation

      c3 = cos rz
      s3 = sin rz
      c2 = cos rx
      s2 = sin rx
      c1 = cos ry
      s1 = sin ry

   -- in colMajor (sx * (c1*c3 + s1*s2*s3)) (sy * (c3*s1*s2 - c1*s3)) (sz * (c2*s1)) x
   --             (sx * (c2*s3))            (sy * (c2*c3))            (sz * (-s2))   y
   --             (sx * (c1*s2*s3 - c3*s1)) (sy * (c1*c3*s2 + s1*s3)) (sz * (c1*c2)) z
   --             0                         0                         0 1
   in unTransform $ translateV tr.position <> rotateY ry <> rotateX rx <> rotateZ rz <> scale3 sx sy sz

