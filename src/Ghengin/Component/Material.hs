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

import Data.Typeable
import GHC.TypeLits
import Data.Kind
import qualified Vulkan as Vk
import qualified Data.IntMap as IM
import qualified Data.Vector as V
import qualified Data.List.NonEmpty as NE
import Data.Hashable
import Ghengin.Asset.Texture
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

  Texture2DBinding :: Texture2D -> Material β -> Material (Texture2D:β)

  -- TODO: Use a unique supply rather than Hashable?

  -- TODO: Rename a few things (DynamicBinding -> DynamicProperty/Dynamic;
  -- StaticBinding -> StaticProperty/Static; TextureProperty/Texture)

-- | All materials for a given pipeline share the same Descriptor Set #1
-- Layout. If we know the pipeline we're creating a material for, we can simply
-- allocate a descriptor set with the known layout for this material.
-- TODO: Add Compatible constraint (first move it to its own module)
material :: Material' α -> RenderPipeline β -> Renderer χ (Material α)
material matf rp = 
  let (_,dpool) NE.:| _ = rp._descriptorSetsSet
   in do
     -- We allocate a descriptor set and create a closure that will give us the
     -- descriptor set when provided with a resource map.
     dsetf <- allocateDescriptorSet 1 dpool

     -- We create a dummy material based on a dummy dset constructed from an
     -- empty set of resources. The descriptor set function constructs a
     -- descriptor set and writes all the resources to their bindings. If we
     -- pass an empty map of resources, nothing is written. So we create an
     -- empty resource map to create a dummy descriptor set to create a material
     -- with a dummy dset so we can inspect its structure and allocate the
     -- required resources that haven't yet been allocated. When we finish doing
     -- so, we'll have constructed the actual resource map, and can then create
     -- a final material.
     dummySet <- dsetf mempty
     let dummyMat = matf dummySet

     -- Make the resource map for this material
     resources <- makeResources dummyMat

     -- Create the descriptor set with the written descriptors based on the
     -- created resource map
     actualDSet <- dsetf resources

     -- Create the material which stores the final descriptor set with the
     -- updated information.
     let actualMat = matf actualDSet

     pure actualMat

-- | Edit a material.
--
-- Most materials are (existentially) stored within a render packet, and thus
-- cannot be edited without checking what material is what, even if we, at the
-- apecs level, only query for entities that we are sure to have that material.
--
-- To introduce a local equality constraint proving that the material we looked
-- up is in fact the one we want to edit, we must use @Data.Typeable@'s @eqT@ function.
--
-- Example
-- @
-- RenderPacket oldMesh (someMaterial :: Material mt) pp _ <- C.get planetEntity
--
-- -- Pattern match on Refl to introduce the mt ~ PlanetMaterial local equality
-- Just Refl <- pure $ eqT @mt @PlanetMaterial
--
-- -- We can now edit the material's second binding because we know it to be a PlanetMaterial material
-- newMat    <- medit @2 @PlanetMaterial someMaterial $ \(WithVec3 x y z) -> vec3 x (y+1) z
-- C.set planetEntity (renderPacket oldMesh newMat pp)
-- @
--
-- The nice thing about introducing equality constraints is that we can edit
-- the material and then re-create the render packet with the same pipeline as
-- it was originally created despite us not knowing anything about its type:
-- The local equality simply allowed us to edit the Material with at specific
-- type, but the information regarding compatibility between *that same type
-- (that we previously didn't know enough about to edit)* is preserved!
--
-- Additionally, when the material is edited through this function
-- resources can be automatically freed and allocated if needed
--
-- Previously we would have to recreate and reallocate all the descriptors and
-- buffers for a material, now we can simply rewrite the exact buffer without
-- doing a single allocation
medit :: ∀ α n χ.  Material α -> (BindingAt n α -> BindingAt n α) -> Renderer χ (Material α)
medit mat update = undefined

type BindingAt :: Nat -> [Type] -> Type
type family BindingAt n α where
  BindingAt n α = Index (Reverse α '[]) n 0 (Text "Failed to get binding #" :<>: ShowType n :<>: Text " of Material " :<>: ShowType α)

-- | Like 'medit' but edit multiple material properties/bindings at the same time
--
-- @
-- @
medit' :: ∀ α ns χ.  Material α -> (HList (BindingsAt ns α) -> HList (BindingsAt ns α)) -> Renderer χ (Material α)
medit' mat update = undefined

type BindingsAt :: [Nat] -> [Type] -> [Type]
type family BindingsAt ns α where
  BindingsAt '[] α = '[]
  BindingsAt (n ': ns) α = BindingAt n α ': BindingsAt ns α

-- | Recursively make the descriptor set resource map from the material. This
-- will create some resources and simply use some provided in the material
-- properties.
-- * Dynamic buffers: It will create a mapped buffer but write nothing to it - these buffers are written every frame.
-- * Static buffer: It will create and write a buffer that can be manually updated
-- * Texture2D: It will simply add the already existing texture that was created (and engine prepared) on texture creation
makeResources :: Material α -> Renderer χ ResourceMap
makeResources m = go (matSizeBindings m - 1) m -- TODO: Fix the order instead of going in reverse... <-- actually, this way it's better? this way when a property is added, its index is fixed and doesn't depend on other properties being added or not
                                                                                                     -- ^ no, it's not that better since the amount of properties is kind of fixed in the type?
  where
    go :: Int -> Material α -> Renderer χ ResourceMap
    go i = \case
      Done _ -> pure mempty

      DynamicBinding x xs -> do

        -- Allocate the associated buffers
        mb <- createMappedBuffer (fromIntegral $ sizeOf x) Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER
        IM.insert i (UniformResource mb) <$> go (i-1) xs

      StaticBinding x xs -> do

        -- Allocate the associated buffers
        mb <- createMappedBuffer (fromIntegral $ sizeOf x) Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER -- TODO: Should this be a deviceLocalBuffer?

        -- Write the static information to this buffer right away
        writeMappedBuffer mb x

        -- TODO: instead -> createDeviceLocalBuffer Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT x

        IM.insert i (UniformResource mb) <$> go (i-1) xs
        
      Texture2DBinding t xs -> do

        -- Image has already been allocated when the texture was created, we
        -- simply pass add it to the resource map
        IM.insert i (Texture2DResource t) <$> go (i-1) xs


-- TODO: IT DOESNT NEED TO BE IN REVERSE ....!!!!!!!! The Material type list can
-- simply be in the order left to right (0 to N bindings)...

-- | Returns the number of bindings
matSizeBindings :: ∀ α. Material α -> Int
matSizeBindings = -- fromInteger $ natVal $ Proxy @(ListSize α)
  \case
    Done _ -> 0
    DynamicBinding _ xs -> 1 + matSizeBindings xs
    StaticBinding _ xs -> 1 + matSizeBindings xs
    Texture2DBinding _ xs -> 1 + matSizeBindings xs

instance Eq (Material '[]) where
  (==) (Done _) (Done _) = True

instance (Eq a, Eq (Material as)) => Eq (Material (a ': as)) where
  (==) (DynamicBinding x xs) (DynamicBinding y ys) = x == y && xs == ys
  (==) (StaticBinding x xs) (StaticBinding y ys) = x == y && xs == ys
  (==) (Texture2DBinding x xs) (Texture2DBinding y ys) = x == y && xs == ys
  (==) _ _ = False

instance Hashable (Material '[]) where
  hashWithSalt i (Done _) = hashWithSalt i ()

instance (Hashable a, Hashable (Material as)) => Hashable (Material (a ': as)) where
  hashWithSalt i (DynamicBinding x xs) = hashWithSalt i x `hashWithSalt` xs
  hashWithSalt i (StaticBinding x xs) = hashWithSalt i x `hashWithSalt` xs
  hashWithSalt i (Texture2DBinding x xs) = hashWithSalt i x `hashWithSalt` xs


materialDescriptorSet :: Material α -> DescriptorSet
materialDescriptorSet = \case
  Done x -> x
  DynamicBinding _ xs -> materialDescriptorSet xs
  StaticBinding _ xs -> materialDescriptorSet xs
  Texture2DBinding _ xs -> materialDescriptorSet xs

freeMaterial :: Material α -> Renderer χ ()
freeMaterial = \case
  Done dset -> pure () -- destroyDescriptorSet dset -- TODO: BIG TODO: Free descriptor set. Should free each binding that should be destroyed (careful with e.g. shared textures)
  StaticBinding _ xs -> freeMaterial xs
  DynamicBinding _ xs -> freeMaterial xs
  Texture2DBinding tex xs -> do
    -- freeTexture tex -- TODO: BIG:TODO: Can't free textures here because they might be shared and we would double free
    freeMaterial xs

