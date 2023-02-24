{-# LANGUAGE OverloadedRecordDot #-}
module Ghengin.Core.Material where

import qualified Apecs
import Control.Lens ((^.), Lens', lens, to)
import Data.Bifunctor
import Data.Unique
import Ghengin.Core.Render.Property
import Ghengin.Core.Type.Compatible
import Ghengin.Core.Render.Pipeline
-- TODO: Remove dependency on Ghengin non-core
import Ghengin.Utils
-- TODO: Remove Vulkan dependency by abstracting over the descriptor set
-- creation and destruction
import Ghengin.Vulkan
import Ghengin.Vulkan.DescriptorSet
import qualified Data.IntMap as IM

{-

Note [Materials]
~~~~~~~~~~~~~~~~

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

type Material' α = Material '[] -> Material α

data Material xs where

  Done :: (DescriptorSet, Unique) -> Material '[] -- The unique key is created from a unique supply in 'material' and the descriptor set passed then.

  MaterialProperty :: ∀ α β
                   .  PropertyBinding α -- ^ A dynamic binding is written (necessarily because of linearity) to a mapped buffer based on the value of the constructor
                   -> Material β
                   -> Material (α:β)

instance Eq (Material '[]) where
  (==) (Done _) (Done _) = True

instance (Eq a, Eq (Material as)) => Eq (Material (a ': as)) where
  (==) (MaterialProperty a xs) (MaterialProperty b ys) = a == b && xs == ys

instance HasProperties Material where

  properties   :: Material α -> PropertyBindings α
  properties = \case
    Done _ -> GHNil
    MaterialProperty x xs -> x :## properties xs

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

data SomeMaterial = ∀ α. SomeMaterial (Material α)
instance Apecs.Component SomeMaterial where
  type Storage SomeMaterial = Apecs.Map SomeMaterial
{-# DEPRECATED material "Material storage should be a cache" #-}

-- | All materials for a given pipeline share the same Descriptor Set #1
-- Layout. If we know the pipeline we're creating a material for, we can simply
-- allocate a descriptor set with the known layout for this material.
material :: ∀ α β ξ χ. CompatibleMaterial' α ξ => Material' α -> RenderPipeline ξ β -> Renderer χ (Material α)
material matf rp = 
  -- TODO: There could be more than 1 descriptor pool in flight (+frames in flight)
  let dpool = rp^.to descriptorPool
   in do

     -- Make the unique identifier for this material
     uniq <- liftIO newUnique

     -- TODO: Merge this "dummy" idea with the one in Core.Render.Pipeline

     -- We bail out early if this descriptor pool has no descriptor sets of
     -- type #1 (which would mean there are no bindings in descriptor set #1
     mat <- case IM.lookup 1 dpool._set_bindings of
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
         resources <- makeResources (properties @_ @α dummyMat)

         -- Create the descriptor set with the written descriptors based on the
         -- created resource map
         actualDSet <- dsetf resources

         -- Create the material which stores the final descriptor set with the
         -- updated information.
         let actualMat = matf $ Done (actualDSet, uniq)

         pure actualMat
     -- TODO: Apecs.newEntity (SomeMaterial mat)
     pure mat


materialUID :: Material α -> Unique
materialUID = \case
  Done x -> snd x
  MaterialProperty _ xs -> materialUID xs

freeMaterial :: Material α -> Renderer χ ()
freeMaterial = \case
  Done x -> destroyDescriptorSet (fst x)
  MaterialProperty prop xs -> freeProperty prop >> freeMaterial xs

