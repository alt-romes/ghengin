-- {-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE LinearTypes #-}
module Ghengin.Component.Transform.Animation where

import qualified Prelude
import Prelude.Linear
import Control.Functor.Linear as Linear
import Data.Typeable

import Apecs.Linear
import Ghengin.Component.Transform

import Geomancy.Vec3
import Ghengin.Core.Renderer.Kernel

import {-# SOURCE #-} Ghengin.World (World)

-- Needs source if we would always call transformAnimationUpdate. For now, every game must call it explicitly.
import {-# SOURCE #-} Ghengin (Ghengin)

-- | An animation constructed from a vectorial movement.
--
-- The 'movement' vector dictates both the length and the direction of the
-- movement ending at 'dest'
data TransformAnimation w = TransformAnimation' { movement  :: Vec3
                                                , dest      :: Vec3
                                                , speed     :: Float -- ^ Speed determines how fast the animation plays
                                                , finalizer :: Ghengin w ()
                                                } -- This is a linear animation across a vector. There could be more complex transform animations later on

-- | Animation:
--
-- The 'movement' vector dictates both the length and the direction starting at
-- the given starting point
transformAnimation :: Vec3 -> Vec3 -> Float -> Ghengin w () -> TransformAnimation w
transformAnimation mov ((Prelude.+mov) -> dest) = TransformAnimation' mov dest

instance Component (TransformAnimation w) where
  type Storage (TransformAnimation w) = Map (TransformAnimation w)

transformAnimationUpdate :: ( Has (World w) Renderer (TransformAnimation w)
                            , Has (World w) Renderer Transform
                            , Typeable w
                            , Dupable w)
                         => Float -- ^ Delta time
                         -> Ghengin w ()
transformAnimationUpdate dt = do
  cmapM $ \(tr :: Transform, TransformAnimation' @w dir dest speed fin) ->
    let -- dest = dir + start we take the dest instead of the start to avoid computing the destination every update
        distLeft = tr.position Prelude.- dest
     in if 1e-3 Prelude.> dot distLeft distLeft -- If we're almost at the destination modulo "large" aproximations
         then Linear.do
           -- Has reached destination
           --   (1) We set the transform to the actual destination to avoid aproximation errors down the line
           --   (2) We remove the animation component so it is no longer updated
           --   (3) Call a finalizer action
           fin
           pure $ Ur (tr{position = dest}, Nothing @(TransformAnimation w))
         else
           -- Has not reached destination
           --   (1) We update the transform by the direction
           --   (2) We keep the animation component
           let newTr = tr{position = tr.position Prelude.+ dir ^* (dt Prelude.* speed)}
            in pure $ Ur (newTr, Just (TransformAnimation' dir dest speed fin))

