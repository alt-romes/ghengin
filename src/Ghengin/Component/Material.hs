{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Ghengin.Component.Material where

import Data.Hashable
import GHC.TypeLits
import Ghengin.Utils

{-

Note [Materials]
~~~~~~~~~~~~~~~~

A material is described by the bindings it requires to be rendered. For
example, a material defined by two textures and a color parameter is described
by two texture bindings and a ...

Materials can only be rendered in compatible render pipelines. That is, any set
of properties given by its bindings describes a material, but to actually
render it we require a shader pipeline that is compatible (TODO: what is being
compatible) with the material properties.

For example, you might define a material with a color and a light-reflection
property, but if the shader program knows nothing about lights or colors, then
that material can't be used in the corresponding pipeline.

Materials are paired with pipelines and assigned to entities through 'RenderPacket'...

   * A 'StaticBinding' writes a descriptor set once (or manually every other time) and simply binds it at render time
  
   * The 'DynamicBinding's are written to the default descriptor set #1 of the
   shader pipeline every draw call based on a given formula to calculate the
   buffer content

     NStaticMaterial :: Material α
     NDynamicMaterial :: [MaterialHasBinding α β => β] -> Material α

   class MaterialHasBinding α β where
     writeMaterial :: α -> MappedBuffer β 1% -> SystemT w (Renderer e) ()

Resources:
* Blender Materials: https://docs.blender.org/manual/en/latest/render/materials/introduction.html
* Material Library File: http://paulbourke.net/dataformats/mtl/

-}

data Material xs where

  Done :: Material '[]

  DynamicBinding :: ∀ α β
                 .  (Storable α, Sized α, Hashable α) -- Storable to write the buffers, Sized to guarantee the instance exists to validate at compile time against the pipeline
                 => α -- ^ A dynamic binding is written (necessarily because of linearity) to a mapped buffer based on the value of the constructor
                 -> Material β
                 -> Material (α:β)
  -- TODO
  -- StaticBinding :: a -> Material a:b

-- | Returns the number of bindings
matSizeBindings :: ∀ α. Material α -> Int
matSizeBindings = -- fromInteger $ natVal $ Proxy @(ListSize α)
  \case
    Done -> 0
    DynamicBinding _ xs -> 1 + matSizeBindings xs

instance Eq (Material '[]) where
  (==) Done Done = True

instance (Eq a, Eq (Material as)) => Eq (Material (a ': as)) where
  (==) (DynamicBinding x xs) (DynamicBinding y ys) = x == y && xs == ys

instance Hashable (Material '[]) where
  hashWithSalt i Done = hashWithSalt i ()

instance (Hashable a, Hashable (Material as)) => Hashable (Material (a ': as)) where
  hashWithSalt i (DynamicBinding x xs) = hashWithSalt i x `hashWithSalt` xs

