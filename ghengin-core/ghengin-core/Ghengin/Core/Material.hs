{-# LANGUAGE OverloadedRecordDot #-}
module Ghengin.Core.Material where

import Data.V.Linear (V,make)
import Ghengin.Core.Prelude as Linear

import Data.Unique ( Unique, newUnique )

import Ghengin.Core.Render.Property
import Ghengin.Core.Type.Compatible ( CompatibleMaterial )
import Ghengin.Core.Render.Pipeline ( RenderPipeline(..) )

import Ghengin.Core.Renderer.Kernel
import Ghengin.Core.Renderer.DescriptorSet

import qualified Data.Linear.Alias as Alias

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

  Done :: (Alias DescriptorSet, Alias ResourceMap, Ur Unique) ⊸ Material '[] -- The unique key is created from a unique supply in 'material' and the descriptor set passed then.

  MaterialProperty :: ∀ α β
                   .  PropertyBinding α -- ^ A dynamic binding is written to a mapped buffer based on the value of the constructor
                    ⊸ Material β
                    ⊸ Material (α:β)

-- Did I need this?
-- instance Eq (Material '[]) where
--   (==) (Done _) (Done _) = True

-- instance (Eq a, Eq (Material as)) => Eq (Material (a ': as)) where
--   (==) (MaterialProperty a xs) (MaterialProperty b ys) = a == b && xs == ys

instance HasProperties Material where

  -- Worry about performance of doing things safely later.
  -- For now, simply strive for correctness.

  properties :: Material α ⊸ Renderer (PropertyBindings α, Material α)
  properties = \case
    Done n -> pure (GHNil, Done n)
    MaterialProperty p0 xs -> Linear.do
      (p1,p2) <- Alias.share p0
      (xs', mat') <- properties xs
      pure (p1 :## xs', MaterialProperty p2 mat')

  descriptors :: Material α ⊸ Renderer (Alias DescriptorSet, Alias ResourceMap, Material α)
  descriptors = \case
    Done (dset0, rmap0, _uq) -> Linear.do
      ((dset1, rmap1), (dset2, rmap2)) <- Alias.share (dset0, rmap0)
      pure (dset1, rmap1, Done (dset2, rmap2, _uq))
    MaterialProperty p xs -> Linear.do
      (dset, rmap, mat') <- descriptors xs
      pure (dset, rmap, MaterialProperty p mat')

  puncons :: Material (α:β) ⊸ (PropertyBinding α, Material β)
  puncons (MaterialProperty p xs) = (p, xs)

  pcons :: PropertyBinding α %p -> Material β ⊸ Material (α:β)
  pcons = MaterialProperty

-- | All materials for a given pipeline share the same Descriptor Set #1
-- Layout. If we know the pipeline we're creating a material for, we can simply
-- allocate a descriptor set with the known layout for this material.
material :: ∀ α π β. CompatibleMaterial α π
         => PropertyBindings α ⊸ RenderPipeline π β ⊸ Renderer (Material α, RenderPipeline π β)
material props0 (RenderProperty pr rps) = material props0 rps >>= \case (m, rp) -> pure (m, RenderProperty pr rp)
material props0 (RenderPipeline gpip rpass (rdset, rres, dpool0) shaders) = Linear.do

  -- Make the unique identifier for this material
  Ur uniq <- liftSystemIOU newUnique

  -- We allocate an empty descriptor set of type #1 to later write with the
  -- resource map
  (dpool1,dset0)  <- Alias.useM dpool0 (\pool -> swap <$> allocateEmptyDescriptorSet 1 pool)
  (dpool2,dpool3) <- Alias.share dpool1

  -- -- We bail out early if this descriptor pool has no descriptor sets of
  -- -- type #1 (which would mean there are no bindings in descriptor set #1
  -- mat <- case IM.lookup 1 dpool._set_bindings of
  --   Nothing -> pure (matf (Done (EmptyDescriptorSet, uniq)))
  --   Just _  -> do

  -- Make the resource map for this material
  -- Will also count texture references
  (resources0, props1) <- makeResources props0

  -- Create the descriptor set with the written descriptors based on the
  -- created resource map
  (dset1, resources1) <- updateDescriptorSet dset0 resources0

  dset2 <- Alias.newAlias (freeDescriptorSets dpool2 . make) dset1
  resources2 <- Alias.newAlias freeResourceMap resources1

  -- Create the material which stores the final descriptor set with the
  -- updated information.
  pure
    ( mkMat (Done (dset2, resources2, Ur uniq)) props1
    , RenderPipeline gpip rpass (rdset, rres, dpool3) shaders
    )
  where
    mkMat :: ∀ b. Material '[] ⊸ PropertyBindings b ⊸ Material b
    mkMat x GHNil = x
    mkMat x (p :## pl) = MaterialProperty p (mkMat x pl)

materialUID :: Material α ⊸ (Ur Unique, Material α)
materialUID = \case
    Done (r,m,Ur y) -> (Ur y, Done (r,m,Ur y))
    MaterialProperty p xs -> case materialUID xs of (uq, mat) -> (uq, MaterialProperty p mat)

freeMaterial :: Material α ⊸ Renderer ()
freeMaterial = \case
  Done (x, y, Ur _) -> Alias.forget x >> Alias.forget y
  MaterialProperty prop xs -> Alias.forget prop >> freeMaterial xs

