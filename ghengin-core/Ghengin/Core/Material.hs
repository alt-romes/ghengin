{-# LANGUAGE OverloadedRecordDot, LinearTypes, QualifiedDo, NoImplicitPrelude #-}
module Ghengin.Core.Material where

import Prelude.Linear

import Data.Unique ( Unique, newUnique )
import Control.Functor.Linear as Linear
import Control.Monad.IO.Class.Linear

import Ghengin.Core.Render.Property
import Ghengin.Core.Type.Compatible ( CompatibleMaterial' )
import Ghengin.Core.Render.Pipeline ( RenderPipeline(..) )

import Ghengin.Core.Renderer.DescriptorSet
import Ghengin.Core.Render.Monad

import Data.Counted as Counted
import qualified Data.Counted.Unsafe as Unsafe.Counted
import qualified Unsafe.Linear as Unsafe

{-

Note [Materials]
~~~~~~~~~~~~~~~~

Update note, a large part now describes render properties

A material is described by the properties it requires to be rendered. For
example, a material defined by two textures and a color parameter could be
described by two texture2D property bindings and one static property binding

Materials can only be rendered in compatible render pipelines. That is, any set
of properties given by its bindings describes a material, but to actually
render it we require a shader pipeline that is compatible (TODO: what is being
compatible) with the material properties.

For example, you might define a material with a color and a light-reflection
property, but if the shader program knows nothing about lights or colors, then
that material can't be used in the pipeline created with that shader.

Materials are paired with meshes and pipelines and assigned to entities through
'RenderPacket'...

Move to Note [Property Bindings]:
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

  Done :: (RefC DescriptorSet, RefC ResourceMap, Ur Unique) ⊸ Material '[] -- The unique key is created from a unique supply in 'material' and the descriptor set passed then.

  MaterialProperty :: ∀ α β
                   .  PropertyBinding α -- ^ A dynamic binding is written (necessarily because of linearity) to a mapped buffer based on the value of the constructor
                   -> Material β
                    ⊸ Material (α:β)

-- Did I need this?
-- instance Eq (Material '[]) where
--   (==) (Done _) (Done _) = True

-- instance (Eq a, Eq (Material as)) => Eq (Material (a ': as)) where
--   (==) (MaterialProperty a xs) (MaterialProperty b ys) = a == b && xs == ys

instance HasProperties Material where

  properties :: Material α ⊸ (Ur (PropertyBindings α), Material α)
  properties = Unsafe.toLinear $ \m -> (unsafeGo m, m) where
    unsafeGo :: Material α -> Ur (PropertyBindings α)
    unsafeGo = \case
      Done {} -> Ur GHNil
      MaterialProperty x xs -> case unsafeGo xs of Ur xs' -> Ur (x :## xs')

  descriptors   :: MonadIO m
                => Material α
                 ⊸ m (RefC DescriptorSet, RefC ResourceMap, Material α)
  descriptors = Unsafe.toLinear (\mat -> unsafeGo mat >>= \case
                                          (dset, rmap) -> pure (dset, rmap, mat)) where
    -- Note it's not linear on the pipeline, unsafe! -- but we return the original reference
    unsafeGo :: MonadIO m => Material α -> m (RefC DescriptorSet, RefC ResourceMap)
    unsafeGo = \case
      Done (dset, rmap, _uq) ->
        -- In descriptors, we're returning the whole render pipeline unchanged.
        -- To return DescriptorSet and ResourceMap we increment their reference
        -- counts because we unsafely keep one reference in the original
        -- renderpipeline we return
        (,) <$> Unsafe.Counted.inc dset <*> Unsafe.Counted.inc rmap

      -- TODO: This will possibly have to become linear
      MaterialProperty _ xs -> unsafeGo xs

  puncons :: Material (α:β) ⊸ (Ur (PropertyBinding α), Material β)
  puncons (MaterialProperty p xs) = (Ur p, xs)

  pcons :: PropertyBinding α %p -> Material β ⊸ Material (α:β)
  pcons = Unsafe.toLinear MaterialProperty

data SomeMaterial = ∀ α. SomeMaterial (Material α)
-- ROMES:TODO: InSTANCE OUTSIDE OF CORE!!!!!
-- instance Apecs.Component SomeMaterial where
--   type Storage SomeMaterial = Apecs.Map SomeMaterial
-- {-# DEPRECATED material "TODO: Material storage should be a cache" #-}

-- | All materials for a given pipeline share the same Descriptor Set #1
-- Layout. If we know the pipeline we're creating a material for, we can simply
-- allocate a descriptor set with the known layout for this material.
material :: ∀ α β π m. (CompatibleMaterial' α π, MonadRenderer m)
         => (Material '[] ⊸ Material α) -> RenderPipeline π β ⊸ m (Material α, RenderPipeline π β)
material matf (RenderProperty pr rps) = material matf rps >>= \case (m, rp) -> pure (m, RenderProperty pr rp)
material matf (RenderPipeline gpip rpass (rdset, rres, dpool0) shaders) = Linear.do

  -- Make the unique identifier for this material
  Ur uniq <- liftSystemIOU newUnique

  -- We allocate an empty descriptor set of type #1 to later write with the
  -- resource map
  (dset0, dpool1) <- allocateEmptyDescriptorSet 1 dpool0

  -- -- We bail out early if this descriptor pool has no descriptor sets of
  -- -- type #1 (which would mean there are no bindings in descriptor set #1
  -- mat <- case IM.lookup 1 dpool._set_bindings of
  --   Nothing -> pure (matf (Done (EmptyDescriptorSet, uniq)))
  --   Just _  -> do

  -- We create a dummy material.
  -- descriptor set and writes all the resources to their bindings. If we
  -- pass an empty map of resources, nothing is written. So we create an
  -- empty resource map to create a dummy descriptor set to create a dummy
  -- material so we can inspect its structure and allocate the required
  -- resources that haven't yet been allocated. When we finish doing so,
  -- we'll have constructed the actual resource map, and can then create a
  -- final material.
  let dummyMat :: Material α = matf $ Done undefined

  -- Make the resource map for this material
  -- Will also count texture references
  resources0 <- makeResources (case properties @_ @α dummyMat of (Ur pbs, _) -> pbs)

  -- Create the descriptor set with the written descriptors based on the
  -- created resource map
  (dset1, resources1) <- updateDescriptorSet dset0 resources0

  dset2 <- Counted.new (error "freeDescriptorSet:RefC needs to be parametrised over monad") dset1
  resources2 <- Counted.new (error "ROMES:TODO!!!!") resources1

  -- Create the material which stores the final descriptor set with the
  -- updated information.
  pure
    ( matf $ Done (dset2, resources2, Ur uniq)
    , RenderPipeline gpip rpass (rdset, rres, dpool1) shaders
    )
  -- TODO: Apecs.newEntity (SomeMaterial mat)

materialUID :: Material α ⊸ (Ur Unique, Material α)
materialUID = Unsafe.toLinear $ \x -> (unsafeGo x, x) where
  unsafeGo :: Material α -> Ur Unique
  unsafeGo = \case
    Done (_,_,y) -> y
    MaterialProperty _ xs -> unsafeGo xs

-- freeMaterial :: Material α -> Renderer χ ()
-- freeMaterial = \case
--   Done x -> destroyDescriptorSet (fst x)
--   MaterialProperty prop xs -> freeProperty prop >> freeMaterial xs

