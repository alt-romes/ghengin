{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- Has w m Material
module Ghengin.Component.Material where

import GHC.Records
import GHC.TypeLits
import Data.IORef
import Data.Kind
import Apecs

import Foreign.Storable

import Ghengin.Render.Pipeline
import Ghengin.Vulkan.Pipeline

import Ghengin.Vulkan.Buffer
import Ghengin.Vulkan
import Ghengin.Utils

-- | Materials.
--
-- * A 'StaticMaterial' writes a descriptor set once (or manually every other time) and simply binds it at render time
--
-- * A 'DynamicMaterial' writes the default descriptor set #1 of the shader
-- pipeline every draw call based on a given formula to calculate the buffer content
-- data Material α where
--   NStaticMaterial :: Material α
--   NDynamicMaterial :: [MaterialHasBinding α β => β] -> Material α

-- class MaterialHasBinding α β where
--   writeMaterial :: α -> MappedBuffer β 1% -> SystemT w (Renderer e) ()

data Material xs where

  Done :: Material '[]

  DynamicBinding :: ∀ α β
                 .  Storable α
                 => α -- ^ A dynamic binding is written (necessarily because of linearity) to a mapped buffer based on the value of the constructor
                 -> Material β
                 -> Material (α:β)

  -- StaticBinding :: a -> Material '[a]
  -- NextStaticBinding :: Material as -> b -> Material (b':as)

data SomeMaterial where
  SomeMaterial  :: ∀ α. Material α -> Int -> SomeMaterial -- ^ Constructs SomeMaterial with the material and its index in the mutable list
--   StaticMaterial  :: ∀ α. Material α => SomeMaterial
--   DynamicMaterial :: ∀ α. Material α => SomeMaterial

data SharedMaterial = SharedMaterial { pipelineIndex :: Int
                                     , materialIndex :: Int
                                     }

-- |  We can only have shared materials be components, because the ones used for rendering are stored in that map
instance (Monad m, HasField "materials" w (Storage SharedMaterial)) => Has w m SharedMaterial where
  getStore = SystemT (asks (.materials))

instance Component SharedMaterial where
  type Storage SharedMaterial = Map SharedMaterial




-- TODO: VALIDATE MATERIAL IN PIPELINE
makeMaterial :: ( PipelineConstraints info tops descs strides
                , HasField "_renderPipelines" ext (IORef [SomeRenderPipeline]) )
             => RenderPipeline info -> Material a -> Renderer ext SharedMaterial
makeMaterial renderPipeline mat = do
  renderPipelinesRef <- asks (._extension._renderPipelines)
  SomeRenderPipeline _ matsRef <- liftIO $ (!! renderPipeline._index) <$> readIORef renderPipelinesRef
  -- TODO: The reference must be shared between the returned material and the saved one
  (length -> matIndex) <- liftIO $ readIORef matsRef
  liftIO $ modifyIORef' matsRef (<> [SomeMaterial mat matIndex])
  pure (SharedMaterial renderPipeline._index matIndex)

-- TODO
-- sameMaterial :: Material α -> Material β -> Bool
-- sameMaterial mat1 mat2 = True

-- warning no validation done on the material being overwritten
writeMaterial :: ( HasField "_renderPipelines" ext (IORef [SomeRenderPipeline]) )
             => SharedMaterial -> Material a -> Renderer ext SharedMaterial
writeMaterial (SharedMaterial pipIx matIx) newMat = do
  renderPipelinesRef <- asks (._extension._renderPipelines)
  SomeRenderPipeline _ matsRef <- liftIO $ (!! pipIx) <$> readIORef renderPipelinesRef
  -- TODO: The reference must be shared between the returned material and the saved one
  liftIO $ modifyIORef' matsRef (map (\(SomeMaterial m ix) -> if ix == matIx then SomeMaterial newMat matIx else SomeMaterial m ix)) -- TODO: use mutable vector here
  pure (SharedMaterial pipIx matIx)

