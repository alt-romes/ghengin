{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExplicitNamespaces #-}
module Ghengin.Core.Render.Property
  ( PropertyBinding(..)
  , PropertyBindings
  , HasProperties(..)
  , HasPropertyAt(..)
  , makeResources
  -- , freeProperty
  , writeProperty

  -- * Utils
  , GHList(..)
  ) where

import Data.Proxy

import qualified Prelude
import Prelude.Linear
import Control.Functor.Linear as Linear
-- TODO: Some special linear lenses to use propertyAt ... import Control.Lens ((^.), Lens', lens)
-- ROMES:TODO: For the lens to be used as a getter, I think we will need this definition of functor rather than the control one.
import GHC.TypeLits ( KnownNat, type (+), Nat, natVal )
import Data.Kind ( Type, Constraint )

import qualified Data.IntMap as IM
import qualified Vulkan as Vk -- TODO: Core shouldn't depend on any specific renderer implementation external to Core
import qualified Unsafe.Linear

import Foreign.Storable (Storable(sizeOf))

import qualified Data.Counted as Counted

import Ghengin.Core.Renderer

data PropertyBinding α where

  DynamicBinding :: ∀ α. (Storable α) -- Storable to write the buffers
                 => α -- ^ A dynamic binding is written (necessarily because of linearity) to a mapped buffer based on the value of the constructor
                 -> PropertyBinding α

  StaticBinding :: ∀ α. (Storable α) -- Storable to write the buffers
                => α -- ^ A dynamic binding is written (necessarily because of linearity) to a mapped buffer based on the value of the constructor
                -> PropertyBinding α

  -- ROMES:TODO: I think the texture needs to be linear and reference counted
  -- ROMES:TODO:!!: Re-add textures!!! Texture2DBinding :: Texture2D -> PropertyBinding Texture2D


instance Eq α => Eq (PropertyBinding α) where
  (==) (DynamicBinding x) (DynamicBinding y) = x == y
  (==) (StaticBinding x)  (StaticBinding y)  = x == y
  -- ROMES:TODO: (==) (Texture2DBinding x) (Texture2DBinding y) = x == y
  (==) x y = Unsafe.Linear.toLinear2 (\_ _ -> False) x y

type PropertyBindings α = GHList PropertyBinding α

-- | Generic HList
data GHList c xs where
    GHNil :: GHList c '[]
    (:##) :: c a -> GHList c as -> GHList c (a ': as)
infixr 6 :##

{-
Note [Coerce HList to List]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
They have the same representation ^_^, so unsafeCoerce is safe ^_^
-}


{-
Note [Property Bindings]
~~~~~~~~~~~~~~~~~~~~~~~~

Materials and Render Pipelines consist of Property Bindings, representing their
properties (be them material or pipeline properties which are bound to the
descriptor set #1 and #0 respectively).

Property bindings map to descriptors in descriptor set.
A property binding can be dynamic, which is a property that will be written to
a descriptor set every frame (by writting the mapped buffer accessible from the
GPU), static, which is a property only written to when explicitly edited, and
texture bindings which work like static bindings but bind textures, a kind of
GPU resource that can be bound on a descriptor set.

We can generate a 'ResourceMap' from a list of property bindings. This resource
map holds resources accessible by the GPU to which we must write in order to
update and set our properties. Most of these resources must be deallocated exactly once.

TODO: Mesh bindings at dset #2

-}

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
makeResources :: ∀ α. PropertyBindings α ⊸ Renderer ResourceMap
makeResources = foldM (\acc (i,x) -> go acc i x) IM.empty . Unsafe.Linear.toLinear2 Prelude.zip [0..] . Unsafe.Linear.coerce -- See Note [Coerce HList to List]
  where
    go :: ∀ β. ResourceMap ⊸ Int ⊸ PropertyBinding β ⊸ Renderer ResourceMap
    go resources i' pb = case pb of
      DynamicBinding x -> Linear.do

        -- Allocate the associated buffers
        mb <- createMappedBuffer (fromIntegral $ sizeOf x) Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER
        pure $ linInsert i' (UniformResource mb) resources

      StaticBinding x -> Linear.do

        -- Allocate the associated buffers
        mb <- createMappedBuffer (fromIntegral $ sizeOf x) Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER -- TODO: Should this be a deviceLocalBuffer?

        -- Write the static information to this buffer right away
        mb' <- writeMappedBuffer mb x

        -- TODO: instead -> createDeviceLocalBuffer Vk.BUFFER_USAGE_UNIFORM_BUFFER_BIT x

        pure $ linInsert i' (UniformResource mb') resources
        
      -- ROMES:TODO:!!! TEXTURES!!
      -- Texture2DBinding t -> Linear.do

      --   -- TODO: Use the linear reference counting library
      --   Unsafe.Linear.toLinear incRefCount t

      --   -- Image has already been allocated when the texture was created, we
      --   -- simply pass add it to the resource map
      --   pure $ linInsert i' (Texture2DResource t) resources

    linInsert :: Int ⊸ a ⊸ IM.IntMap a ⊸ IM.IntMap a
    linInsert = Unsafe.Linear.toLinear3 IM.insert

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
writeProperty :: RefC MappedBuffer ⊸ PropertyBinding α -> Renderer (RefC MappedBuffer)
writeProperty buf = \case
  StaticBinding  _ ->
    -- Already has been written to, we simply bind it together with the rest of
    -- the set at draw time and do nothing here.
    pure buf
  -- ROMES:TODO:TEXTURES
  -- Texture2DBinding  _ ->
  --   -- As above. Static bindings don't get written every frame.
  --   pure buf
  DynamicBinding (a :: α) ->
    -- Dynamic bindings are written every frame
    -- writeMappedBuffer @α buf a
    writeMappedBuffer buf a
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
  properties    :: φ α ⊸ (Ur (PropertyBindings α), φ α)
  descriptors   :: φ α ⊸ Renderer (RefC DescriptorSet, RefC ResourceMap, φ α)
  puncons       :: φ (α:β) ⊸ (Ur (PropertyBinding α), φ β) -- ROMES:TODO: This is not gonna be Ur when we re-add textures
  pcons         :: PropertyBinding α %p -> φ β ⊸ φ (α:β)

-- | If we know that a type (φ α) has property of type (β) at binding (#n), we
-- can edit that property or get its value
--
-- Instanced by Material, Mesh? and RenderPipeline
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
  --
  -- A short note on the linearity of the Lens and resource safety: This is a
  -- lens from a material or render pipeline into the *value* of a property,
  -- not its binding resource. Therefore, with the lens it is impossible to
  -- duplicate a resource, so it's safe for the value to be operated on
  -- unrestrictedly and, for the same reason, be returned as an unrestricted
  -- value in the renderer monad. However(!), we cannot duplicate the
  -- material-like φ, since φ itself holds a descriptor set that must be used
  -- linearly -- so the modified type of the outer structure does not see φ as
  -- an unrestricted value; The resulting function on the outer structure must
  -- have a multiplicity of 1, since otherwise we won't be able to use it on
  -- materials that must be used linearly.
  --
  -- ROMES:TODO: What about the functor? Does it need to be linear? Can it even
  -- be linear?  I'm not sure a normal functor could make references escape,
  -- but I think we can get the same results using the linear DATA functor
  -- which doesn't let things escape for sure.
  --
  -- One can think of the type of the function as:
  -- propertyAt :: Lens (φ α) (Renderer (φ α)) β (Renderer (Ur β))
  --
  -- This is almost like a lens, but while the we use the linear control
  -- functor rather than the data one, we won't be able to use this lens as a
  -- getter
  propertyAt :: ∀ γ ρ χ. Linear.Functor γ => (β %ρ -> γ (Renderer (Ur β))) %χ -> (φ α ⊸ γ (Renderer (φ α)))

instance (HasPropertyAt' n 0 φ α β, HasProperties φ) => HasPropertyAt n β φ α where
  propertyAt = propertyAt' @n @0 @φ @α @β

-- | Helper class to instance 'HasPropertyAt'.
-- 
-- There is a default implementation for 'HasPropertyAt' and instances are only
-- required for the 'HasProperties' class
class HasPropertyAt' n m φ α β where
  propertyAt' :: Linear.Functor f => (β %p -> f (Renderer (Ur β))) %x -> (φ α ⊸ f (Renderer (φ α)))

-- TODO: Instance with type error for "No available property with type X at position N"

-- This instance should always overlap the recursive instance below because we
-- want to stop when we find the binding
instance {-# OVERLAPPING #-}
  ( HasProperties φ
  , KnownNat n
  ) => HasPropertyAt' n n φ (β:αs) β where

  -- propertyAt' :: MonadRenderer μ => Lens (φ (β:αs)) (μ (φ (β:αs))) β (μ (Ur β))
  propertyAt' :: ∀ ρ χ γ. Linear.Functor γ => (β %ρ -> γ (Renderer (Ur β))) %χ -> (φ (β:αs) ⊸ γ (Renderer (φ (β:αs))))
  propertyAt' afmub s   =
    case puncons s of -- ft
      -- TODO: prop might have to be linear
      (Ur prop, xs0)      ->
        (\mub ->
          descriptors xs0 >>= \case
            (dset, resmap, xs1) -> edit prop dset resmap xs1 mub
        ) Linear.<$> afmub (propertyValue prop)
            -- (\b -> pcons <$> editProperty prop (const b) (fromIntegral (natVal $ Proxy @n)) (xs ^. descriptorSet) <*> pure xs)
   where
    edit :: PropertyBinding β ⊸ RefC DescriptorSet ⊸ RefC ResourceMap ⊸ φ αs ⊸ Renderer (Ur β) ⊸ Renderer (φ (β:αs))
    edit prop dset resmap xs mub = Linear.do
      -- Ur b <- mub
      -- TODO: Perhaps assert this isn't the last usage of dset and resmap,
      -- though I imagine it would be quite hard to get into that situation.
      (dset'  , freeDSet)   <- Counted.get dset
      (resmap', freeResMap) <- Counted.get resmap
      (updatedProp, dset'', resmap'') <- editProperty prop (const mub) (nat @n) dset' resmap'
      freeDSet dset''
      freeResMap resmap''
      pure $ pcons updatedProp xs

    propertyValue :: PropertyBinding α ⊸ α
    propertyValue = \case
      DynamicBinding x -> x
      StaticBinding x -> x
      -- Texture2DBinding x -> x

nat :: ∀ m. KnownNat m => Int
nat = fromIntegral (natVal $ Proxy @m)

instance {-# OVERLAPPABLE #-}
  ( HasProperties φ
  , HasPropertyAt' n (m+1) φ αs β
  ) => HasPropertyAt' n m φ (α ': αs) β where

  -- propertyAt' :: MonadRenderer μ => Lens (φ (α:αs)) (μ (φ (α:αs))) β (μ (Ur β))
  propertyAt' :: ∀ f p x. Linear.Functor f => (β %p -> f (Renderer (Ur β))) %x -> (φ (α:αs) ⊸ f (Renderer (φ (α:αs))))
  propertyAt' f x =
    case puncons x of
      (Ur prop, xs) ->
        fmap (pcons prop) <$> propertyAt' @n @(m+1) @φ @αs @β f xs
    
-- Does it make sense to have this?
-- instance
--   ( Length α ~ m
--   , TypeError (Text "Failed to get property binding #" :<>: ShowType n :<>: Text " from properties " :<>: ShowType α)
--   ) => HasPropertyAt' n m φ α b where
--     propertyAt' = undefined

-- | Edit the value of a property. You most likely don't need this function.
-- See 'HasPropertyAt'.
editProperty :: ∀ α p
              . PropertyBinding α    -- ^ Property to edit/update
              ⊸ (α %p -> Renderer (Ur α))    -- ^ Update function
              ⊸ Int                  -- ^ Property index in descriptor set
             -> DescriptorSet   -- ^ The descriptor set with corresponding index and property resources
              ⊸ ResourceMap     -- ^ The descriptor set with corresponding index and property resources
              ⊸ Renderer (PropertyBinding α, DescriptorSet, ResourceMap) -- ^ Returns the updated property binding
editProperty prop update i dset resmap0 = Linear.do
  case prop of
    DynamicBinding x -> Linear.do
      Ur ux <- update x

      (bufref, resmap1) <- getUniformBuffer resmap0 i

      -- TODO: Move this logic inside writeMappedBuffer and make its type RefC MappedBuffer ⊸ a -> lm (RefC MappedBuffer)
      -- (buf, freeBuf) <- Counted.get bufref

      writeDynamicBinding bufref ux >>= Counted.forget

      --freeBuf mb -- We know here there will be more than one reference since we also return the resource map
      --           -- but does it matter? if this really were the last reference to the mapped buffer it should indeed be freed.
      --           -- if it so happens that this is not the last reference but we actually free it here then our unsafe bits of reference counting are wrong
      --           --
      --           -- ROMES:Note: the 'free' name is a bit counter intuitive, we will never really free it here...
      --           -- TODO: Give it a better name

      pure (DynamicBinding ux, dset, resmap1)

    StaticBinding x -> Linear.do
      Ur ux <- update x

      (bufref, resmap1) <- getUniformBuffer resmap0 i

      -- (buf, freeBuf) <- Counted.get bufref

      writeStaticBinding bufref ux >>= Counted.forget

      -- freeBuf mb -- Same as above, we aren't really "freeing", this is a bad name

      pure (StaticBinding ux, dset, resmap1)

    -- ROMES:TODO: Textures!!!!!!!!!!
    -- Texture2DBinding x -> Linear.do
    --   Ur ux <- update x

    --   updateTextureBinding dset ux

    --   -- We free the texture that was previously bound
    --   freeTexture x

    --   -- We increase the texture reference count that was just now bound
    --   incRefCount ux

    --   pure $ Texture2DBinding ux

  where
    writeDynamicBinding :: Storable α => RefC MappedBuffer ⊸ α -> Renderer (RefC MappedBuffer)
    writeDynamicBinding = writeMappedBuffer @α

    -- TODO: For now, static bindings use a mapped buffer as well, but perhaps
    -- it'd be better to use a GPU local buffer to which we write only so often
    writeStaticBinding :: Storable α => RefC MappedBuffer ⊸ α -> Renderer (RefC MappedBuffer)
    writeStaticBinding = writeMappedBuffer @α

    -- | Overwrite the texture bound on a descriptor set at binding #n
    --
    -- TODO: Is it OK to overwrite previously written descriptor sets at specific points?
    -- TODO: this one has the potential to be wrong, think about it carefully eventually
    -- ROMES:TODO: Textures!!!!
    -- updateTextureBinding :: DescriptorSet ⊸ Texture2D -> μ (DescriptorSet, ResourceMap)
    -- updateTextureBinding dset = updateDescriptorSet dset . IM.singleton i . Texture2DResource

-- ROMES:TODO
-- freeProperty :: MonadRenderer μ => PropertyBinding α -> μ ()
-- freeProperty = \case
--   DynamicBinding _ -> pure ()
--   StaticBinding _ -> pure ()
--   Texture2DBinding x -> freeTexture x

