{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module Ghengin.Core.Render.Property
  ( PropertyBinding(..)
  , PropertyBindings
  , HasProperties(..)
  , HasPropertyAt(..)
  , BufferType (..)
  , makeResources
  , writeProperty

  -- * Utils
  , GHList(..)
  ) where

import Data.Proxy
import GHC.TypeError
import Ghengin.Core.Log
import Ghengin.Core.Prelude as Linear
import Ghengin.Core.Renderer.Kernel
import Ghengin.Core.Renderer
import Ghengin.Core.Render
import Ghengin.Core.Type.Utils
import qualified Data.Linear.Alias as Alias
import qualified Data.IntMap.Linear as IM
import Vulkan.Linear ()

import qualified Vulkan as Vk -- TODO: Core shouldn't depend on any specific renderer implementation external to Core

-- |
--
-- === Example
--
-- If your game had a static camera which never moved, or rarely did, you
-- should use a 'StaticBinding' to bind the camera matrix property. On the
-- other hand, if your game had a first-person-camera which was updated every
-- frame, you should use a 'DynamicBinding'.
data PropertyBinding α where

-- NB: Currently, we use std140 for uniform & storage buffers, but this could
-- eventually be std430 if we used VK_KHR_uniform_buffer_standard_layout. These
-- layouts are provided by Block from gl-block

  -- | Write the property to a mapped buffer every frame
  DynamicBinding :: ∀ α
                  . ( Block α  -- Block to write the buffers with proper standard
                    , PropertyIn α ~ α
                    , PropertyOut α ~ Ur α ) -- When we match here we learn this
                 => α -- ^ A dynamic binding is written to a mapped buffer based on the value of the constructor every frame
                 -> PropertyBinding α

  -- | A static buffer is re-used (without being written to) every frame to bind this property.
  -- The static buffer is written when the property is edited, and only then.
  -- If you want data that updates frequently, use a 'DynamicBinding' instead
  StaticBinding :: ∀ α
                 . ( Block α  -- Block to write the buffers with proper standard
                   , PropertyIn α ~ α
                   , PropertyOut α ~ Ur α ) -- When we match here we learn this
                => α -> PropertyBinding α

  Texture2DBinding :: Alias (Texture2D fmt) ⊸ PropertyBinding (Texture2D fmt)


instance Forgettable Renderer (PropertyBinding α) where
  forget = \case
    DynamicBinding _ -> pure ()
    StaticBinding  _ -> pure ()
    Texture2DBinding refc -> Alias.forget refc

instance MonadIO m => Shareable m (PropertyBinding α) where
  share = \case
    DynamicBinding x -> pure (DynamicBinding x, DynamicBinding x)
    StaticBinding  x -> pure (StaticBinding x, StaticBinding x)
    Texture2DBinding t -> bimap Texture2DBinding Texture2DBinding <$> Alias.share t

-- | A 'PropertyBinding' actual value. Useful when we want to define functions
-- over the value bound when constructing the PropertyBinding rather than the
-- type that shows up in the typelist.
--
-- For all intents and purposes, this is the inverse of 'PropertyBinding'.
-- type family PBInv α = r | r -> α where
--   PBInv (Texture2D fmt) = Alias (Texture2D fmt)
--   PBInv x               = Ur x

type family PropertyMult α = r where
  PropertyMult (Texture2D fmt) = 'One
  PropertyMult x               = 'Many

type family PropertyIn α = r where
  PropertyIn (Texture2D fmt) = Alias (Texture2D fmt)
  PropertyIn x               = x

type family PropertyOut α = r where
  PropertyOut (Texture2D fmt) = Alias (Texture2D fmt)
  PropertyOut x               = Ur x

type PropertyBindings α = GHList PropertyBinding α

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

Likewise we have Mesh property bindings at dset #2

-}

-- | Recursively make the descriptor set resource map from the list of properties. This
-- will create some resources
--
-- * Dynamic buffers: It will create a mapped buffer but write nothing to it - these buffers are written every frame.
-- * Static buffer: It will create and write a buffer that can be manually updated
-- * Texture2D: It will simply add the already existing texture that was created (and engine prepared) on texture creation
makeResources :: ∀ α. BindingsMap -> PropertyBindings α ⊸ Renderer (ResourceMap, PropertyBindings α)
makeResources bm = enterD "makeResources" . go_build 0 bm
  where
    go_build :: ∀ αs. Int -> BindingsMap -> PropertyBindings αs ⊸ Renderer (ResourceMap, PropertyBindings αs)
    go_build !_i _bmap GHNil        = pure (IM.empty, GHNil)
    go_build !i bmap (pb :## pbs) = Linear.do
      (dres, pb')  <- go (fromMaybe (error "Impossible makeResources") (IM.lookup i bmap)) pb
      (rmap, pbs') <- go_build (i+1) bmap pbs
      pure (IM.insert i dres rmap, pb' :## pbs')

    bufferType :: Vk.DescriptorType -> BufferType
    bufferType Vk.DESCRIPTOR_TYPE_UNIFORM_BUFFER = Uniform
    bufferType Vk.DESCRIPTOR_TYPE_STORAGE_BUFFER = Storage
    bufferType t = error $ "Invalid descriptor buffer type: " ++ show t

    dRes :: BufferType -> Alias MappedBuffer ⊸ DescriptorResource
    dRes Uniform mb = UniformResource mb
    dRes Storage mb = StorageResource mb

    go :: ∀ β. (Vk.DescriptorType, Vk.ShaderStageFlags) -> PropertyBinding β ⊸ Renderer (DescriptorResource, PropertyBinding β)
    go (dt, _stage) pb =
      let bt = bufferType dt
      in case pb of
        DynamicBinding x -> Linear.do

          -- Allocate the associated buffers. These buffers will be written to
          -- every frame (unlike buffers underlying `StaticBinding`s)
          mb <- createMappedBuffer (fromIntegral $ sizeOf140 (Proxy @β)) bt

          pure (dRes bt mb, DynamicBinding x)

        StaticBinding x -> Linear.do
          -- Allocate the associated buffers
          -- TODO: This be a deviceLocalBuffer
          -- TODO: instead -> createDeviceLocalBuffer bt x
          mb <- createMappedBuffer (fromIntegral $ sizeOf140 (Proxy @β)) bt 

          -- Write the static information to this buffer right away. It may be
          -- later updated if the static property is edited with `editProperty`.
          mb' <- writeMappedBuffer mb x

          pure (dRes bt mb', StaticBinding x)

        Texture2DBinding t -> Linear.do

          (t1, t2) <- Alias.share t

          -- Image has already been allocated when the texture was created, we
          -- simply share it to the resource map
          pure (Texture2DResource t1, Texture2DBinding t2)

-- | Write a property binding value to a mapped buffer.  Eventually we might
-- want to associate the binding set and binding #number and get them directly
-- from the mapped buffers
--
--  For each binding
--    (1.1) If it's dynamic, write the buffer
--    (1.2) If it's static, do nothing because the buffer is already written
--    (1.3) If it's a texture, do nothing because the texture is written only once and has already been bound
--
-- The property bindings function should be created from a compatible pipeline
writeProperty :: DescriptorResource ⊸ PropertyBinding α ⊸ Renderer (DescriptorResource, PropertyBinding α)
writeProperty dr pb = case pb of
  StaticBinding x ->
    -- The static binding is static, and has already has been written to (at
    -- initialisation and eventually `editProperty`). We simply bind it
    -- together with the rest of the set at draw time and do nothing here.
    pure (dr, StaticBinding x)
  Texture2DBinding t ->
    -- As above. Static bindings don't get written every frame.
    pure (dr, Texture2DBinding t)
  DynamicBinding (a :: α) ->
    case dr of
      UniformResource buf -> Linear.do
        -- Dynamic bindings are written every frame
        buf' <- writeMappedBuffer buf a
        pure (UniformResource buf', DynamicBinding a)
      StorageResource buf -> Linear.do
        buf' <- writeMappedBuffer buf a
        pure (StorageResource buf', DynamicBinding a)
      Texture2DResource t -> Alias.forget t >>
        error "writeProperty: one can't write a dynamic binding into a non-mapped-buffer resource"


-- | Class of types that have property bindings.
-- Instances include 'Mesh', 'Material' and 'RenderPipeline'.
--
-- We can fetch their descriptor set resources, as well as edit their
-- individual properties through 'HasPropertyAt'.
--
-- Consider as an alternative to HasProperties a list-like type of properties
-- with an χ parameter for the extra information at the list's end.
class HasProperties φ where
  -- Re-think these...?
  properties    :: φ α ⊸ Renderer (PropertyBindings α, φ α)
  descriptors   :: φ α ⊸ Renderer (Alias DescriptorSet, Alias ResourceMap, φ α)
  puncons       :: φ (α:β) ⊸ (PropertyBinding α, φ β)
  pcons         :: PropertyBinding α ⊸ φ β ⊸ φ (α:β)

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
  -- ROMES:TODO: I suppose I no longer can have %p here instead of %1? Try thinking about it again...
  -- propertyAt :: ∀ γ ρ χ. Linear.Functor γ => (β %ρ -> γ (Renderer β)) %χ -> (φ α ⊸ γ (Renderer (φ α)))
  propertyAt :: ((PropertyIn β) %(PropertyMult β) -> Renderer (PropertyOut β)) %χ -> (φ α ⊸ Renderer (φ α))
                     -- ^ linearity here enforces correct freeing of linear property!

instance (HasPropertyAt' n 0 φ α β, HasProperties φ) => HasPropertyAt n β φ α where
  propertyAt = propertyAt' @n @0 @φ @α @β

-- | Helper class to instance 'HasPropertyAt'.
-- 
-- There is a default implementation for 'HasPropertyAt' and instances are only
-- required for the 'HasProperties' class
class HasPropertyAt' n m φ α β where
  propertyAt' :: (PropertyIn β %(PropertyMult β) -> Renderer (PropertyOut β)) %x -> (φ α ⊸ Renderer (φ α))

instance (Unsatisfiable (Text "The requested property " :<>: ShowType α :<>: Text " does not exist in the properties list" {- :<>: ShowType α -}))
         => HasPropertyAt' n n' φ '[] α where
  propertyAt' = unsatisfiable

-- This instance should always overlap the recursive instance below because we
-- want to stop when we find the binding
instance {-# OVERLAPPING #-}
  ( HasProperties φ
  , KnownNat n
  ) => HasPropertyAt' n n φ (β:αs) β where

  propertyAt' :: (PropertyIn β %(PropertyMult β) -> Renderer (PropertyOut β)) %χ -> (φ (β:αs) ⊸ Renderer (φ (β:αs)))
  propertyAt' afmub s =
    case puncons s of
      (prop, xs0)      ->
        descriptors xs0 Linear.>>= \case
          (dset, resmap, xs1) -> edit prop dset resmap xs1 afmub
   where
    edit :: PropertyBinding β ⊸ Alias DescriptorSet ⊸ Alias ResourceMap ⊸ φ αs ⊸ (PropertyIn β %(PropertyMult β) -> Renderer (PropertyOut β)) ⊸ Renderer (φ (β:αs))
    edit prop dset resmap xs fmub = Linear.do
      -- Ur b <- mub
      (dset'  , freeDSet)   <- Alias.get dset
      (resmap', freeResMap) <- Alias.get resmap
      (updatedProp, dset'', resmap'') <- editProperty prop fmub (nat @n) dset' resmap'
      freeDSet dset''
      freeResMap resmap''
      pure $ pcons updatedProp xs

instance {-# OVERLAPPABLE #-}
  ( HasProperties φ
  , HasPropertyAt' n (m+1) φ αs β
  ) => HasPropertyAt' n m φ (α ': αs) β where

  -- propertyAt' :: MonadRenderer μ => Lens (φ (α:αs)) (μ (φ (α:αs))) β (μ (Ur β))
  -- propertyAt' :: ∀ f p x. Linear.Functor f => (β %1 -> f (Renderer β)) %x -> (φ (α:αs) ⊸ f (Renderer (φ (α:αs))))
  propertyAt' :: (PropertyIn β %(PropertyMult β) -> Renderer (PropertyOut β)) %x -> (φ (α:αs) ⊸ Renderer (φ (α:αs)))
  propertyAt' f x =
    case puncons x of
      (prop, xs) ->
        pcons prop <$> propertyAt' @n @(m+1) @φ @αs @β f xs

-- | Edit the value of a property. You most likely don't need this function.
-- See 'HasPropertyAt'.
editProperty :: ∀ α m
              . PropertyBinding α    -- ^ Property to edit/update
              ⊸ (PropertyIn α %(PropertyMult α) -> Renderer (PropertyOut α))    -- ^ Update function
              ⊸ Int                  -- ^ Property index in descriptor set
             -> DescriptorSet   -- ^ The descriptor set with corresponding index and property resources
              ⊸ ResourceMap     -- ^ The descriptor set with corresponding index and property resources
              ⊸ Renderer (PropertyBinding α, DescriptorSet, ResourceMap) -- ^ Returns the updated property binding
editProperty prop update i dset resmap0 = Linear.do
  case prop of
    DynamicBinding x -> Linear.do
      Ur ux <- update x

      -- We don't need to do the update to a *dynamic* property on editProperty
      -- since `writeProperty` always writes the mapped buffer of dynamic
      -- properties, every frame.
      pure (DynamicBinding ux, dset, resmap0)

    StaticBinding x -> Linear.do
      Ur ux <- update x

      -- Oh. Had to change this because the pattern wasn't complete so
      -- linearity stopped being valid. That's an improvement!
      getDescriptorResource resmap0 i >>= \case
        (UniformResource bufref, resmap1) -> Linear.do

          writeStaticBinding bufref ux >>= Alias.forget

          pure (StaticBinding ux, dset, resmap1)

        (StorageResource bufref, resmap1) -> Linear.do

          writeStaticBinding bufref ux >>= Alias.forget

          pure (StaticBinding ux, dset, resmap1)

        (Texture2DResource t, resmap1) ->
          Alias.forget resmap1 >> Alias.forget t >>
          error "is this right?" dset

    Texture2DBinding xalias -> Linear.do
      ux <- update xalias -- Update function is the one taking into account the
                          -- previous texture as an aliased value. It might free it and return a new
                          -- reference counted texture, for example.

      (ux1, ux2) <- Alias.share ux

      -- We don't need to do anything besides updating the texture binding with
      -- the updated texture, regardless of what the update function did with
      -- the reference counted value. Linearity guarantees it will handle it
      -- correctly.
      dset1 <- updateTextureBinding dset ux1

      pure (Texture2DBinding ux2, dset1, resmap0)

  where
    -- TODO: For now, static bindings use a mapped buffer as well, but it'd be
    -- better to use a GPU local buffer to which we write only so often (see
    -- makeResources and createDeviceLocalBuffer)
    writeStaticBinding :: Block α => Alias MappedBuffer ⊸ α -> Renderer (Alias MappedBuffer)
    writeStaticBinding = writeMappedBuffer @α

    -- | Overwrite the texture bound on a descriptor set at binding #n
    --
    -- TODO: Is it OK to overwrite previously written descriptor sets at specific points?
    --  I think not, because we don't have per-frame descriptor sets, right?
    --    TODO: per-frame descriptor sets.
    updateTextureBinding :: forall fmt. DescriptorSet ⊸ Alias (Texture2D fmt) ⊸ Renderer DescriptorSet
    updateTextureBinding dset' t
      = updateDescriptorSet dset' (IM.insert i (Texture2DResource t) IM.empty) >>=
        (\(dset'', rmap) -> Linear.do
          -- We can forget the single texture2D references in the resource map
          -- since we only shared it to be able to update the resource map with it.
          case IM.elems rmap of
            [Texture2DResource tex] -> Alias.forget tex
            x -> Alias.forget x >> error "updateTextureBinding: not expecting any resource other than a single texture2d resource here."
          pure dset'')

