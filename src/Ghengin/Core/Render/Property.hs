{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Ghengin.Core.Render.Property where

import Control.Lens ((^.), Lens, Lens', lens)
import GHC.TypeLits
import Data.Kind
import Data.Foldable
-- TODO: Remove dependency on Ghengin non-core
import Ghengin.Asset.Texture
import Ghengin.Utils
-- TODO: Remove dependency on Vulkan
import Ghengin.Vulkan (Renderer)
import Ghengin.Vulkan.Buffer
import Ghengin.Vulkan.DescriptorSet
import qualified Data.IntMap as IM
import qualified Vulkan as Vk -- TODO: Core shouldn't depend on any specific renderer implementation external to Core
import qualified Unsafe.Coerce

data PropertyBinding α where

  DynamicBinding :: ∀ α. (Storable α) -- Storable to write the buffers
                 => α -- ^ A dynamic binding is written (necessarily because of linearity) to a mapped buffer based on the value of the constructor
                 -> PropertyBinding α

  StaticBinding :: ∀ α. (Storable α) -- Storable to write the buffers
                => α -- ^ A dynamic binding is written (necessarily because of linearity) to a mapped buffer based on the value of the constructor
                -> PropertyBinding α

  Texture2DBinding :: Texture2D -> PropertyBinding Texture2D


instance Eq α => Eq (PropertyBinding α) where
  (==) (DynamicBinding x) (DynamicBinding y) = x == y
  (==) (StaticBinding x)  (StaticBinding y)  = x == y
  (==) (Texture2DBinding x) (Texture2DBinding y) = x == y
  (==) _ _ = False

type PropertyBindings α = GHList PropertyBinding α

propertyValue :: PropertyBinding α -> α
propertyValue = \case
  DynamicBinding x -> x
  StaticBinding x -> x
  Texture2DBinding x -> x

-- | Recursively make the descriptor set resource map from the list of properties. This
-- will create some resources
--
-- * Dynamic buffers: It will create a mapped buffer but write nothing to it - these buffers are written every frame.
-- * Static buffer: It will create and write a buffer that can be manually updated
-- * Texture2D: It will simply add the already existing texture that was created (and engine prepared) on texture creation
--
-- Additionally, update the reference counts of resources that are reference
-- counted:
--  * Texture2D
makeResources :: ∀ α χ. PropertyBindings α -> Renderer χ ResourceMap
makeResources = foldrM (\(i,x) acc -> go acc i x) mempty . zip [0..] . Unsafe.Coerce.unsafeCoerce -- See Note [Coerce HList to List]
  where
    go :: ∀ β χ1. ResourceMap -> Int -> PropertyBinding β -> Renderer χ1 ResourceMap
    go resources i = \case
      DynamicBinding x -> do

        -- Allocate the associated buffers
        mb <- createMappedBuffer (fromIntegral $ sizeOf x) Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER
        pure $ IM.insert i (UniformResource mb) resources

      StaticBinding x -> do

        -- Allocate the associated buffers
        mb <- createMappedBuffer (fromIntegral $ sizeOf x) Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER -- TODO: Should this be a deviceLocalBuffer?

        -- Write the static information to this buffer right away
        writeMappedBuffer mb x

        -- TODO: instead -> createDeviceLocalBuffer Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT x

        pure $ IM.insert i (UniformResource mb) resources
        
      Texture2DBinding t -> do

        incRefCount t

        -- Image has already been allocated when the texture was created, we
        -- simply pass add it to the resource map
        pure $ IM.insert i (Texture2DResource t) resources


-- | Write a property binding value to a mapped buffer.  Eventually we might
-- want to associate the binding set and binding #number and get them directly
-- from the mapped buffers
--
-- (1) For each binding
--    (1.1) If it's dynamic, write the buffer
--    (1.2) If it's static, do nothing because the buffer is already written
--    (1.3) If it's a texture, do nothing because the texture is written only once and has already been bound
--
-- The property bindings function should be created from a compatible pipeline
writeProperty :: MappedBuffer -> PropertyBinding α -> Renderer χ ()
writeProperty buf = \case
  StaticBinding  _ ->
    -- Already has been written to, we simply bind it together with the rest of
    -- the set at draw time and do nothing here.
    pure ()
  Texture2DBinding  _ ->
    -- As above. Static bindings don't get written every frame.
    pure ()
  DynamicBinding (a :: α) ->
    -- Dynamic bindings are written every frame
    writeMappedBuffer @α buf a
{-# INLINE writeProperty #-}


-- | Class of types that have property bindings.
-- Instances include 'Mesh', 'Material' and 'RenderPipeline'.
--
-- We can fetch their descriptor set resources, as well as edit their
-- individual properties through 'HasPropertyAt'.
--
-- Consider as an alternative to HasProperties a list-like type of properties
-- with an χ parameter for the extra information at the list's end.
class HasProperties φ where
  properties    :: φ α -> PropertyBindings α
  descriptorSet :: Lens' (φ α) DescriptorSet
  puncons       :: φ (α:β) -> (PropertyBinding α, φ β)
  pcons         :: PropertyBinding α -> φ β -> φ (α:β)

-- | If we know that a type (φ α) has property of type (β) at binding (#n), we
-- can edit that property or get its value
--
-- Instanced by Material, Mesh and RenderPipeline
type HasPropertyAt :: Nat              -- ^ Position at which the structure has the property
                   -> Type             -- ^ Property it has at the position
                   -> ([Type] -> Type) -- ^ Structure with list of properties
                   -> [Type]           -- ^ Type level list of properties
                   -> Constraint
class HasProperties φ => HasPropertyAt n β φ α where

  -- | Lens to get and edit a φ that holds a list of bindings.
  -- φ might be a 'Material', a 'RenderPipeline', or a 'Mesh'
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
  -- -- We can now edit the material's second binding using the 'propertyAt'
  -- -- lens, because we know the material to be a PlanetMaterial
  -- newMat    <- someMaterial & propertyAt @2 %~ \(WithVec3 x y z) -> pure (vec3 x (y+1) z)
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
  propertyAt :: Lens (φ α) (Renderer χ (φ α)) β (Renderer χ β)

instance (HasPropertyAt' n 0 φ α β, HasProperties φ) => HasPropertyAt n β φ α where
  propertyAt = propertyAt' @n @0 @φ @α @β

-- | Helper class to instance 'HasPropertyAt'.
-- 
-- There is a default implementation for 'HasPropertyAt' and instances are only
-- required for this class
class HasPropertyAt' n m φ α β where
  propertyAt' :: Lens (φ α) (Renderer χ (φ α)) β (Renderer χ β)

-- This instance should always overlap the recursive instance below because we
-- want to stop when we find the binding
instance {-# OVERLAPPING #-}
  ( HasProperties φ
  , KnownNat n
  ) => HasPropertyAt' n n φ (β:αs) β where

  propertyAt' :: Lens (φ (β:αs)) (Renderer χ (φ (β:αs))) β (Renderer χ β)
  propertyAt' = lens get' set' where

    get' :: φ (β:αs) -> β
    get' = propertyValue . fst . puncons

    set' :: φ (β:αs) -> Renderer χ β -> Renderer χ (φ (β:αs))
    set' (puncons -> (prop, xs)) rb =
      pcons <$>
        editProperty prop (const rb) (fromIntegral (natVal $ Proxy @n)) (xs ^. descriptorSet) <*>
          pure xs

instance {-# OVERLAPPABLE #-}
  ( HasProperties φ
  , HasPropertyAt' n (m+1) φ αs β
  ) => HasPropertyAt' n m φ (α ': αs) β where

  propertyAt' :: Lens (φ (α:αs)) (Renderer χ (φ (α:αs))) β (Renderer χ β)
  propertyAt' f (puncons -> (prop, xs)) =
    fmap (pcons prop) <$> propertyAt' @n @(m+1) @φ @αs @β f xs
    
-- Does it make sense to have this?
-- instance
--   ( Length α ~ m
--   , TypeError (Text "Failed to get property binding #" :<>: ShowType n :<>: Text " from properties " :<>: ShowType α)
--   ) => HasPropertyAt' n m φ α b where
--     propertyAt' = undefined

-- | Edit the value of a property. You most likely don't need this function.
-- See 'pedit' and 'peditM' in 'HasPropertyAt'
editProperty :: ∀ α χ
              . PropertyBinding α   -- ^ Property to edit/update
             -> (α -> Renderer χ α) -- ^ Update function
             -> Int                 -- ^ Property index in descriptor set
             -> DescriptorSet       -- ^ The descriptor set with corresponding index and property resources
             -> Renderer χ (PropertyBinding α) -- ^ Returns the updated property binding
editProperty prop update i dset = case prop of
    DynamicBinding x -> do
      ux <- update x
      writeDynamicBinding ux
      pure $ DynamicBinding ux
    StaticBinding x -> do
      ux <- update x
      writeStaticBinding ux
      pure $ StaticBinding ux
    Texture2DBinding x -> do
      ux <- update x
      updateTextureBinding ux

      -- We free the texture that was previously bound
      freeTexture x
      -- We increase the texture reference count that was just now bound
      incRefCount ux

      pure $ Texture2DBinding ux
  where
    writeDynamicBinding :: Storable α => α -> Renderer χ ()
    writeDynamicBinding = writeMappedBuffer @α (getUniformBuffer dset i)

    -- For now, static bindings use a mapped buffer as well
    writeStaticBinding :: Storable α => α -> Renderer χ ()
    writeStaticBinding = writeMappedBuffer @α (getUniformBuffer dset i)

    -- | Overwrite the texture bound on a descriptor set at binding #n
    --
    -- TODO: Is it OK to overwrite previously written descriptor sets at specific points?
    updateTextureBinding :: Texture2D -> Renderer χ ()
    updateTextureBinding = updateDescriptorSet (dset._descriptorSet) . IM.singleton i . Texture2DResource

freeProperty :: PropertyBinding α -> Renderer χ ()
freeProperty = \case
  DynamicBinding _ -> pure ()
  StaticBinding _ -> pure ()
  Texture2DBinding x -> freeTexture x

