module Ghengin.Render.Property where

import Data.Foldable
import Ghengin.Asset.Texture
import Ghengin.Utils
import Ghengin.Vulkan (Renderer)
import Ghengin.Vulkan.Buffer
import Ghengin.Vulkan.DescriptorSet
import qualified Data.IntMap as IM
import qualified Vulkan as Vk
import qualified Unsafe.Coerce

data PropertyBinding α where

  DynamicBinding :: ∀ α. (Storable α, Sized α) -- Storable to write the buffers, Sized to guarantee the instance exists to validate at compile time against the pipeline
                 => α -- ^ A dynamic binding is written (necessarily because of linearity) to a mapped buffer based on the value of the constructor
                 -> PropertyBinding α

  StaticBinding :: ∀ α. (Storable α, Sized α) -- Storable to write the buffers, Sized to guarantee the instance exists to validate at compile time against the pipeline
                => α -- ^ A dynamic binding is written (necessarily because of linearity) to a mapped buffer based on the value of the constructor
                -> PropertyBinding α

  Texture2DBinding :: Texture2D -> PropertyBinding Texture2D

instance Eq α => Eq (PropertyBinding α) where
  (==) (DynamicBinding x) (DynamicBinding y) = x == y
  (==) (StaticBinding x)  (StaticBinding y)  = x == y
  (==) (Texture2DBinding x) (Texture2DBinding y) = x == y
  (==) _ _ = False

type PropertyBindings α = GHList PropertyBinding α

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



