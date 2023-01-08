-- {-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
module Ghengin.Component.Transform.Animation where

import GHC.Records

import Data.Typeable

import Ghengin.Component
import Ghengin.Component.Transform

import Geomancy.Vec3

-- Needs source if we would always call transformAnimationUpdate. For now, every game must call it explicitly.
import Ghengin (Ghengin)

-- | An animation constructed from a vectorial movement.
--
-- The 'movement' vector dictates both the length and the direction of the
-- movement ending at 'dest'
data TransformAnimation w = TransformAnimation' { movement  :: Vec3
                                                , dest      :: Vec3
                                                , finalizer :: Ghengin w ()
                                                } -- This is a linear animation across a vector. There could be more complex transform animations later on

-- | Animation:
--
-- The 'movement' vector dictates both the length and the direction starting at
-- the given starting point
transformAnimation :: Vec3 -> Vec3 -> Ghengin w () -> TransformAnimation w
transformAnimation mov ((+mov) -> dest) = TransformAnimation' mov dest

instance Component (TransformAnimation w) where
  type Storage (TransformAnimation w) = Map (TransformAnimation w)

instance (Monad m, HasField "transformAnimations" w (Storage (TransformAnimation w))) => Has w m (TransformAnimation w) where
  getStore = SystemT (asks (.transformAnimations))

transformAnimationUpdate :: (HasField "transformAnimations" w (Storage (TransformAnimation w)), HasField "transforms" w (Storage Transform), Typeable w)
                         => Float -- ^ Speed
                         -> Float -- ^ Delta time
                         -> Ghengin w ()
transformAnimationUpdate speed dt = do
  cmapM $ \(tr :: Transform, TransformAnimation' @w dir dest fin) ->
    let -- dest = dir + start we take the dest instead of the start to avoid computing the destination every update
        distLeft = tr.position - dest
     in if 1e-4 > dot distLeft distLeft -- If we're almost at the destination modulo "large" aproximations
         then do
           -- Has reached destination
           --   (1) We set the transform to the actual destination to avoid aproximation errors down the line
           --   (2) We remove the animation component so it is no longer updated
           --   (3) Call a finalizer action
           fin
           pure (tr{position = dest}, Nothing @(TransformAnimation w))
         else
           -- Has not reached destination
           --   (1) We update the transform by the direction
           --   (2) We keep the animation component
           let newTr = tr{position = tr.position + dir ^* (dt * speed)}
            in pure (newTr, Just (TransformAnimation' dir dest fin))
