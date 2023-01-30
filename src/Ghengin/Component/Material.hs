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

import Data.Unique
import Data.Typeable
import GHC.TypeLits
import qualified Vulkan as Vk
import qualified Data.IntMap as IM
import qualified Data.List.NonEmpty as NE
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

type Material' α = Material '[] -> Material α

data Material xs where

  Done :: (DescriptorSet, Unique) -> Material '[] -- The unique key is created from a unique supply in 'material' and the descriptor set passed then.

  DynamicBinding :: ∀ α β
                 .  (Storable α, Sized α) -- Storable to write the buffers, Sized to guarantee the instance exists to validate at compile time against the pipeline
                 => α -- ^ A dynamic binding is written (necessarily because of linearity) to a mapped buffer based on the value of the constructor
                 -> Material β
                 -> Material (α:β)

  StaticBinding :: ∀ α β
                .  (Storable α, Sized α) -- Storable to write the buffers, Sized to guarantee the instance exists to validate at compile time against the pipeline
                => α -- ^ A dynamic binding is written (necessarily because of linearity) to a mapped buffer based on the value of the constructor
                -> Material β
                -> Material (α:β)

  Texture2DBinding :: Texture2D -> Material β -> Material (Texture2D:β)

-- Use a unique supply rather than Hashable

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
         let dummyMat = matf $ Done (dummySet, uniq)

         -- Make the resource map for this material
         -- Will also count texture references
         resources <- makeResources dummyMat

         -- Create the descriptor set with the written descriptors based on the
         -- created resource map
         actualDSet <- dsetf resources

         -- Create the material which stores the final descriptor set with the
         -- updated information.
         let actualMat = matf $ Done (actualDSet, uniq)

         pure actualMat


class HasBindingsAt ns α βs where
  getBindingValues :: Material α -> HList βs

  -- | Like 'medit' but edit multiple material properties/bindings at the same time
  --
  -- @
  -- Update bindings #0 and #2
  -- newMaterial <- medits @PlanetMaterial @[0,2] mat $ (\_oldMinMax -> newMinMax) :# (\_oldTex -> newTex) :# HNil
  -- @
  --
  -- Updates are done from left to right in the list of updates, if that matters
  medits :: Material α -> HFList βs -> Renderer χ (Material α)

instance HasBindingsAt '[] α '[] where
  getBindingValues _ = HNil
  medits mat _ = pure mat

instance (HasBindingAt n α β, HasBindingsAt ns α βs) => HasBindingsAt (n ': ns) α (β ': βs) where
  getBindingValues mat = getBindingValue @n @α @β mat :# getBindingValues @ns @α @βs mat

  medits mat (update :-# updates) = do
    newMat <- medit @n @α @β mat update
    medits @ns @α @βs newMat updates

  medits mat (update :+# updates) = do
    newMat <- meditM @n @α @β mat update
    medits @ns @α @βs newMat updates

class HasBindingAt n α β where
  getBindingValue :: Material α -> β

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
  -- resources can be automatically managed if needed
  --
  -- Previously we would have to recreate and reallocate all the descriptors and
  -- buffers for a material, now we can simply rewrite the exact buffer without
  -- doing a single allocation, or update the dset with the new texture
  --
  -- Another great thing, previously we would need to allocate a new descriptor
  -- set every time we wanted to edit a material, but we didn't discard it
  -- because freeing descriptor sets is actually freeing the pool (or using a
  -- specific slower flag for freeing individual sets if i'm not mistaken).
  -- This way, we always re-use the same set by simply writing over the e.g.
  -- texture bindings if need be
  medit :: Material α -> (β -> β) -> Renderer χ (Material α)

  -- | Like 'medit' but with monadic computation
  meditM :: Material α -> (β -> Renderer χ β) -> Renderer χ (Material α)


instance HasBindingAt' (n+1) (Length α) α β => HasBindingAt n α β where
  getBindingValue = getBindingValue' @(n+1) @(Length α) @α @β
  medit mat update = medit' @(n+1) @(Length α) @α @β mat (pure . update)
  meditM mat update = medit' @(n+1) @(Length α) @α @β mat update

class HasBindingAt' n m a b where
  getBindingValue' :: Material a -> b
  medit' :: Material a -> (b -> Renderer χ b) -> Renderer χ (Material a)

instance TypeError (Text "Failed to get binding #" :<>: ShowType (n-1) :<>: Text " of Material " :<>: ShowType α)
  => HasBindingAt' n 0 α b where
  getBindingValue' = undefined
  medit' = undefined

-- This instance should always overlap the recursive instance below because we
-- want to stop when we find the binding
instance {-# OVERLAPPING #-} KnownNat n => HasBindingAt' n n (b ': as) b where
  getBindingValue' = headMat

  medit' mat update = case mat of
    DynamicBinding x xs -> do
      ux <- update x
      writeDynamicBinding ux ((fromIntegral $ natVal $ Proxy @n) - 1) (materialDescriptorSet xs)
      pure $ DynamicBinding ux xs
    StaticBinding x xs -> do
      ux <- update x
      writeStaticBinding ux ((fromIntegral $ natVal $ Proxy @n) - 1) (materialDescriptorSet xs)
      pure $ StaticBinding ux xs
    Texture2DBinding x xs -> do
      ux <- update x
      updateTextureBinding ux ((fromIntegral $ natVal $ Proxy @n) - 1) (materialDescriptorSet xs)

      -- We free the texture that was previously bound
      freeTexture x
      -- We increase the texture reference count that was just now bound
      incRefCount ux

      pure $ Texture2DBinding ux xs
    

instance {-# OVERLAPPABLE #-} HasBindingAt' n (m-1) as b => HasBindingAt' n m (a ': as) b where
  getBindingValue' = \case
    DynamicBinding _ xs -> getBindingValue' @n @(m-1) @as @b xs
    StaticBinding  _ xs -> getBindingValue' @n @(m-1) @as @b xs
    Texture2DBinding  _ xs -> getBindingValue' @n @(m-1) @as @b xs
  medit' mat update = case mat of
    DynamicBinding x xs    -> DynamicBinding x   <$> medit' @n @(m-1) @as @b xs update
    StaticBinding  x xs    -> StaticBinding x    <$> medit' @n @(m-1) @as @b xs update
    Texture2DBinding  x xs -> Texture2DBinding x <$> medit' @n @(m-1) @as @b xs update

headMat :: ∀ α β. Material (α ': β) -> α
headMat = \case
  DynamicBinding x _ -> x
  StaticBinding  x _ -> x
  Texture2DBinding  x _ -> x

tailMat :: ∀ α β. Material (α ': β) -> Material β
tailMat = \case
  DynamicBinding _ xs -> xs
  StaticBinding  _ xs -> xs
  Texture2DBinding  _ xs -> xs

writeDynamicBinding :: ∀ α χ. Storable α => α -> Int -> DescriptorSet -> Renderer χ ()
writeDynamicBinding a i dset = writeMappedBuffer @α (getUniformBuffer dset i) a

-- For now, static bindings use a mapped buffer as well
writeStaticBinding :: ∀ α χ. Storable α => α -> Int -> DescriptorSet -> Renderer χ ()
writeStaticBinding a i dset = writeMappedBuffer @α (getUniformBuffer dset i) a

-- | Overwrite the texture bound on a descriptor set at binding #n
--
-- TODO: Is it OK to overwrite previously written descriptor sets at specific points?
updateTextureBinding :: Texture2D -> Int -> DescriptorSet -> Renderer χ ()
updateTextureBinding tex i dset = updateDescriptorSet (dset._descriptorSet) (IM.singleton i (Texture2DResource tex))


-- | Recursively make the descriptor set resource map from the material. This
-- will create some resources and simply use some provided in the material
-- properties.
-- * Dynamic buffers: It will create a mapped buffer but write nothing to it - these buffers are written every frame.
-- * Static buffer: It will create and write a buffer that can be manually updated
-- * Texture2D: It will simply add the already existing texture that was created (and engine prepared) on texture creation
--
-- Additionally, update the reference counts of resources that are reference
-- counted:
--  * Texture2D
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

        incRefCount t

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


materialDescriptorSet :: Material α -> DescriptorSet
materialDescriptorSet = \case
  Done x -> fst x
  DynamicBinding _ xs -> materialDescriptorSet xs
  StaticBinding _ xs -> materialDescriptorSet xs
  Texture2DBinding _ xs -> materialDescriptorSet xs

getMaterialUID :: Material α -> Unique
getMaterialUID = \case
  Done x -> snd x
  DynamicBinding _ xs -> getMaterialUID xs
  StaticBinding _ xs -> getMaterialUID xs
  Texture2DBinding _ xs -> getMaterialUID xs

freeMaterial :: Material α -> Renderer χ ()
freeMaterial = \case
  Done x -> destroyDescriptorSet (fst x)
  StaticBinding _ xs -> freeMaterial xs
  DynamicBinding _ xs -> freeMaterial xs
  Texture2DBinding tex xs -> do
    freeTexture tex
    freeMaterial xs


-- Heterogenous list of functions
-- We use this instaed of the below function in Material to get better type
-- inference
data HFList xs where
    HFNil :: HFList '[]
    (:-#) :: (a -> a) -> HFList as -> HFList (a ': as)
    (:+#) :: (forall χ. a -> Renderer χ a) -> HFList as -> HFList (a ': as)
infixr 6 :-#
infixr 6 :+#
-- type family FunctionsOn xs where
--   FunctionsOn '[] = '[]
--   FunctionsOn (x ': xs) = (x -> x) ': FunctionsOn xs

