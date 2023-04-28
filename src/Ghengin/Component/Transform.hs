{-# OPTIONS_GHC -Wno-orphans #-} -- Transform instances
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Ghengin.Component.Transform
  ( Transform(..)
  , noTransform
  , applyTransform
  , makeTransform
  ) where

import Control.Monad.IO.Class

import Geomancy hiding (Transform)
import Geomancy.Mat4
import qualified Vulkan as Vk
import Apecs

import Ghengin.Vulkan.Renderer.Command
import Ghengin.Vulkan.Renderer.Pipeline

data Transform = Transform { position :: {-# UNPACK #-} !Vec3
                           , scale    :: {-# UNPACK #-} !Vec3
                           , rotation :: {-# UNPACK #-} !Vec3
                           -- , mat      :: Mat4 -- This field must be lazy so that's computed only when needed. Then it'll be computed/cached so subsequent accesses will be instantaneous
                           }

noTransform :: Transform
noTransform = Transform (vec3 0 0 0) (vec3 1 1 1) (vec3 0 0 0)

-- TODO: Abstract data type that caches the resulting transformation, and
-- provides an update operation that updates always updates the cache

instance Component Transform where
  type Storage Transform = Map Transform

applyTransform :: MonadIO m => VulkanPipeline -> Transform -> RenderPassCmd m
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

   in let w = colMajor (sx * (c1*c3 + s1*s2*s3)) (sy * (c3*s1*s2 - c1*s3)) (sz * (c2*s1)) x
                       (sx * (c2*s3))            (sy * (c2*c3))            (sz * (-s2))   y
                       (sx * (c1*s2*s3 - c3*s1)) (sy * (c1*c3*s2 + s1*s3)) (sz * (c1*c2)) z
                       0                         0                         0 1
       in w
   -- With a fresh head this morning I followed the clipping planes line of
   -- thought and eventually found that the translation was being affected by
   -- the rotation (I'll send a snippet after) and with correct transformation
   -- matrix and values for the z offset the rotation worked and wasn't being
   -- clipped. The key insight was that the issue was the clipping planes and
   -- then I was able to focus on that which lead to a solution
   --
   -- If I multiply the homogeneous translation matrix translateV tr.position
   -- by the rest of the transformation matrix I was expecting to get a
   -- transformation matrix that also translates... but my reasoning there
   -- appears to be wrong. If someone is able to explain it clearly I'd love to
   -- know :)
   -- in let w = unTransform $ translateV tr.position <> rotateY ry <> rotateX rx <> rotateZ rz <> scale3 sx sy sz
   -- in let w = unTransform $ scale3 sx sy sz <> rotateZ rz <> rotateX rx <> rotateY ry <> translateV tr.position -- this works but seems to rotate to the wrong side?
   --     in w
   -- in traceShow (tr.position, tr.scale, tr.rotation) $ unTransform $ rotateY ry <> scale3 sx sy sz

