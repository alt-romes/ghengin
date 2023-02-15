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

import Control.Lens ((^.), Lens', Lens, lens)
import Data.Bifunctor
import Data.Typeable
import Data.Unique
import GHC.TypeLits
import Ghengin.Asset.Texture
import Ghengin.Render.Pipeline
import Ghengin.Core.Render.Property
import Ghengin.Utils
import Ghengin.Vulkan
import Ghengin.Vulkan.DescriptorSet
import qualified Data.IntMap as IM
import qualified Data.List.NonEmpty as NE
import Unsafe.Coerce

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

type Material' α = Material '[] -> Material α

data Material xs where

  Done :: (DescriptorSet, Unique) -> Material '[] -- The unique key is created from a unique supply in 'material' and the descriptor set passed then.

  MaterialProperty :: ∀ α β
                   .  PropertyBinding α -- ^ A dynamic binding is written (necessarily because of linearity) to a mapped buffer based on the value of the constructor
                   -> Material β
                   -> Material (α:β)


-- | All materials for a given pipeline share the same Descriptor Set #1
-- Layout. If we know the pipeline we're creating a material for, we can simply
-- allocate a descriptor set with the known layout for this material.
-- TODO: Add Compatible constraint (first move it to its own module)
material :: ∀ α β ξ χ. Material' α -> RenderPipeline β ξ -> Renderer χ (Material α)
material matf rp = 
  let (_,dpool) NE.:| _ = rp^.descriptorSetsSet
   in do

     -- Make the unique identifier for this material
     uniq <- liftIO newUnique

     -- We bail out early if this descriptor pool has no descriptor sets of
     -- type #1 (which would mean there are no bindings in descriptor set #1
     case IM.lookup 1 dpool._set_bindings of
       Nothing -> pure (matf (Done (EmptyDescriptorSet, uniq)))
       Just _  -> do

         -- We allocate a descriptor set of type #1 and create a closure that will
         -- give us the descriptor set when provided with a resource map.
         dsetf <- allocateDescriptorSet 1 dpool

         -- We create a dummy material based on a dummy dset constructed from an
         -- empty set of resources. The descriptor set function constructs a
         -- descriptor set and writes all the resources to their bindings. If we
         -- pass an empty map of resources, nothing is written. So we create an
         -- empty resource map to create a dummy descriptor set to create a dummy
         -- material so we can inspect its structure and allocate the required
         -- resources that haven't yet been allocated. When we finish doing so,
         -- we'll have constructed the actual resource map, and can then create a
         -- final material.
         dummySet <- dsetf mempty
         let dummyMat :: Material α = matf $ Done (dummySet, uniq)

         -- Make the resource map for this material
         -- Will also count texture references
         resources <- makeResources ((unsafeCoerce . reverse . unsafeCoerce) $ materialProperties @α dummyMat)

         -- Create the descriptor set with the written descriptors based on the
         -- created resource map
         actualDSet <- dsetf resources

         -- Create the material which stores the final descriptor set with the
         -- updated information.
         let actualMat = matf $ Done (actualDSet, uniq)

         pure actualMat

-- | Returns the number of bindings
matSizeBindings :: ∀ α. Material α -> Int
matSizeBindings = -- fromInteger $ natVal $ Proxy @(ListSize α)
  \case
    Done _ -> 0
    MaterialProperty _ xs -> 1 + matSizeBindings xs

instance Eq (Material '[]) where
  (==) (Done _) (Done _) = True

instance (Eq a, Eq (Material as)) => Eq (Material (a ': as)) where
  (==) (MaterialProperty a xs) (MaterialProperty b ys) = a == b && xs == ys

-- | Note the properties are returned using the reverse order that I DEFINITELY need to fix because it complicates everything.
materialProperties :: Material α -> PropertyBindings α
materialProperties = \case
  Done _ -> GHNil
  MaterialProperty x xs -> x :## materialProperties xs

getMaterialUID :: Material α -> Unique
getMaterialUID = \case
  Done x -> snd x
  MaterialProperty _ xs -> getMaterialUID xs

freeMaterial :: Material α -> Renderer χ ()
freeMaterial = \case
  Done x -> destroyDescriptorSet (fst x)
  MaterialProperty (StaticBinding  _    ) xs -> freeMaterial xs
  MaterialProperty (DynamicBinding _    ) xs -> freeMaterial xs
  MaterialProperty (Texture2DBinding tex) xs -> do
    freeTexture tex
    freeMaterial xs

instance HasProperties Material where

  descriptorSet :: Lens' (Material α) DescriptorSet
  descriptorSet = lens get' set' where
    get' :: Material α -> DescriptorSet
    get' = \case
      Done x -> fst x
      MaterialProperty _ xs -> get' xs
      
    set' :: Material α -> DescriptorSet -> Material α
    set' m d = case m of
      Done x -> Done $ first (const d) x
      MaterialProperty x m' -> MaterialProperty x (set' m' d)

  puncons :: Material (α:β) -> (PropertyBinding α, Material β)
  puncons (MaterialProperty p xs) = (p, xs)

  pcons :: PropertyBinding α -> Material β -> Material (α:β)
  pcons = MaterialProperty
