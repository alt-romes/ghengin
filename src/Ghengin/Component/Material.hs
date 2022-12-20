{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- Has w m Material
module Ghengin.Component.Material where

import GHC.Records
import GHC.TypeLits
import Data.Kind
import Apecs

import Ghengin.Render.Pipeline

import Ghengin.Vulkan.Buffer
import Ghengin.Utils

-- | Materials.
--
-- * A 'StaticMaterial' writes a descriptor set once (or manually every other time) and simply binds it at render time
--
-- * A 'DynamicMaterial' writes the default descriptor set #1 of the shader
-- pipeline every draw call based on a given formula to calculate the buffer content
data Material α where
  NStaticMaterial :: Material α
  NDynamicMaterial :: [MaterialHasBinding α β => β] -> Material α

class MaterialHasBinding α β where
  writeMaterial :: α -> MappedBuffer β 1% -> SystemT w (Renderer e) ()

data Material xs where
  Binding :: a -> Material '[a]
  AndBinding :: Material as -> b -> Material (b':as)



-- TODO: Try to avoid this existential bc how am I going to validate these
-- functions taken as input? Maybe add them iteratively in which case the above
-- material would'nt look quite like that
--
-- For now it's a dirty hack...
-- data SomeFormula = ∀ α w e. SomeFormula (MappedBuffer α %1 -> SystemT w (Renderer e) )
data SomeFormula = ∀ a. SomeFormula a

data SomeMaterial where
  StaticMaterial  :: ∀ α. Material α => SomeMaterial
  DynamicMaterial :: ∀ α. Material α => SomeMaterial

instance (Monad m, HasField "materials" w (Storage SomeMaterial)) => Has w m SomeMaterial where
  getStore = SystemT (asks (.materials))

instance Component SomeMaterial where
  type Storage SomeMaterial = Map SomeMaterial




makeDynamicMaterial :: RenderPipeline info -> 
makeDynamicMaterial = undefined


-- TODO
sameMaterial :: Material α -> Material β -> Bool
sameMaterial mat1 mat2 = True


