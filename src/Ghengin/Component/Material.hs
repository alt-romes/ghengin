{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LinearTypes #-}
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

import qualified Data.List.NonEmpty as NE
import Data.Hashable
import Ghengin.Render.Pipeline
import Ghengin.Vulkan.Buffer
import Ghengin.Vulkan.DescriptorSet
import Ghengin.Vulkan
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

type Material' α = DescriptorSet -> Material α

data Material xs where

  Done :: DescriptorSet -> Material '[]

  DynamicBinding :: ∀ α β
                 .  (Storable α, Sized α, Hashable α) -- Storable to write the buffers, Sized to guarantee the instance exists to validate at compile time against the pipeline, Hashable for the unique key
                 => α -- ^ A dynamic binding is written (necessarily because of linearity) to a mapped buffer based on the value of the constructor
                 -> Material β
                 -> Material (α:β)
  StaticBinding :: ∀ α β
                .  (Storable α, Sized α, Hashable α) -- Storable to write the buffers, Sized to guarantee the instance exists to validate at compile time against the pipeline, Hashable for the unique key
                => α -- ^ A dynamic binding is written (necessarily because of linearity) to a mapped buffer based on the value of the constructor
                -> Material β
                -> Material (α:β)
  -- TODO: Use a unique supply rather than Hashable.


-- | All materials for a given pipeline share the same Descriptor Set #1
-- Layout. If we know the pipeline we're creating a material for, we can simply
-- allocate a descriptor set with the known layout for this material.
-- TODO: Add Compatible constraint (first move it to its own module)
material :: Material' α -> RenderPipeline β -> Renderer χ (Material α)
material mat' rp = 
  let (_,dpool) NE.:| _ = rp._descriptorSetsSet
   in do
     dset <- allocateDescriptorSet 1 dpool
     let mat = mat' dset
     writeStaticBindings (matSizeBindings mat - 1) dset mat -- TODO: here it's in reverse...
     pure mat

-- TODO: IT DOESNT NEED TO BE IN REVERSE ....!!!!!!!! The Material type list can
-- simply be in the order left to right (0 to N bindings)...

writeStaticBindings :: Int -- ^ Which binding # we're currently writing (it keeps decrementing as it recurses)
                    -> DescriptorSet
                    -> Material α
                    -> Renderer χ ()
writeStaticBindings n dset = \case
  Done _ -> pure ()
  DynamicBinding _ xs -> writeStaticBindings (n-1) dset xs
  StaticBinding a as -> do
    writeMappedBuffer (getBindingBuffer dset n) a
    writeStaticBindings (n-1) dset as


-- | Returns the number of bindings
matSizeBindings :: ∀ α. Material α -> Int
matSizeBindings = -- fromInteger $ natVal $ Proxy @(ListSize α)
  \case
    Done _ -> 0
    DynamicBinding _ xs -> 1 + matSizeBindings xs
    StaticBinding _ xs -> 1 + matSizeBindings xs

instance Eq (Material '[]) where
  (==) (Done _) (Done _) = True

instance (Eq a, Eq (Material as)) => Eq (Material (a ': as)) where
  (==) (DynamicBinding x xs) (DynamicBinding y ys) = x == y && xs == ys
  (==) (StaticBinding x xs) (StaticBinding y ys) = x == y && xs == ys
  (==) (StaticBinding _ _) (DynamicBinding _ _) = False
  (==) (DynamicBinding _ _) (StaticBinding _ _) = False

instance Hashable (Material '[]) where
  hashWithSalt i (Done _) = hashWithSalt i ()

instance (Hashable a, Hashable (Material as)) => Hashable (Material (a ': as)) where
  hashWithSalt i (DynamicBinding x xs) = hashWithSalt i x `hashWithSalt` xs
  hashWithSalt i (StaticBinding x xs) = hashWithSalt i x `hashWithSalt` xs


materialDescriptorSet :: Material α -> DescriptorSet
materialDescriptorSet = \case
  Done x -> x
  DynamicBinding _ xs -> materialDescriptorSet xs
  StaticBinding _ xs -> materialDescriptorSet xs

freeMaterial :: Material α -> Renderer χ ()
freeMaterial = \case
  Done dset -> destroyDescriptorSet dset
  StaticBinding _ xs -> freeMaterial xs
  DynamicBinding _ xs -> freeMaterial xs

