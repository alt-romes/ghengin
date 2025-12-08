{-# LANGUAGE UndecidableInstances #-}
{-|
The @CompatibleX@ type constraint validates that a group of properties of @X@
are compatible with a given shader pipeline info, where @X@ can be @'Vertex'@,
@'Mesh'@ @'Material'@ and @'RenderPipeline'@.

@'Compatible'@ validates the vertices types, the material properties, and the
render pipeline properties are compatible with the shader pipeline.
 -}
module Ghengin.Core.Type.Compatible
  ( Compatible
  , CompatibleVertex
  , CompatibleMesh
  , CompatibleMaterial
  , CompatiblePipeline
  ) where

import Prelude
import GHC.TypeError
import Data.Kind ( Type, Constraint )
import FIR.Pipeline
    ( BindingStrides,
      PipelineInfo(..),
      PrimitiveTopology,
      VertexLocationDescriptions,
      GetVertexInputInfo )
import FIR.ProgramState
    ( EntryPointInfo(EntryPointInfo), TLInterfaceVariable )
import SPIRV.Decoration (Decoration(..))
import qualified SPIRV.Image as SPIRV

import Ghengin.Core.Shader.Data hiding (SizeOf) -- TODO: ultimately, we don't want to hide SizeOf, and want to get rid of Sized
import Ghengin.Core.Type.Utils
import Ghengin.Core.Type.Sized

------- Compatible ---------------------------------

-- | 'Compatible' validates at the type level that the mesh and material are
-- compatible with the render pipeline. See Note [Pipeline compatible materials].
type Compatible :: [Type]       -- ^ Vertex properties
                -> [Type]       -- ^ Mesh properties
                -> [Type]       -- ^ Material properties
                -> [Type]       -- ^ Render properties
                -> PipelineInfo -- ^ The pipeline against which the properties must be compatible
                -> Constraint
type family Compatible αs δs βs ξs π where
  Compatible as bs cs ds p
    = ( CompatibleVertex   as p
      , CompatibleMesh     bs p
      , CompatibleMaterial cs p
      , CompatiblePipeline ds p
      )

-- | 'CompatibleVertex' validates the vertices types against the vertices the
-- shader pipeline expects.
type CompatibleVertex as p
      = ( MatchPropertiesSize as (InputLocations p)
                              (Text " mesh vertex properties in vertex " :<>: ShowType as)
                              (Text " inputs in the vertex shader.")
        , CompatibleVertex' (Zip (NumbersFromTo 0 (Length as)) as) p
        )
type CompatibleVertex' :: [(Nat,Type)] -> PipelineInfo -> Constraint
type family CompatibleVertex' as p where
  CompatibleVertex' '[] _ = ()
  CompatibleVertex' ('(n,x) ': xs) p
    = ( ShaderData x
      , Match (SizeOf (FirType x)) (SizeOf (InputByLocation' n p))
                        (Text "Vertex property #" :<>: ShowType n :<>: TypeAndInternalType x
                          :<>: Text " whose internal type is " :<>: ShowType (FirType x)
                          :<>: Text " isn't compatible with the shader vertex property #" :<>: ShowType n :<>: Text " of type " :<>: ShowType (InputByLocation' n p))
      , CompatibleVertex' xs p
      )

-- | 'CompatibleMesh' validates the mesh properties againsts the
-- properties expected in the descriptor set #2 by the shader pipeline.
type CompatibleMesh ds p
      = ( MatchPropertiesSize ds (DSetBindings 2 p)
                              (Text " mesh properties in mesh " :<>: ShowType ds)
                              (Text " descriptors in descriptor set #2.")
        , CompatibleMesh' (Zip (NumbersFromTo 0 (Length ds)) ds) p
        )
type CompatibleMesh' :: [(Nat,Type)] -> PipelineInfo -> Constraint
type family CompatibleMesh' as p where
  CompatibleMesh' '[] _ = ()
  CompatibleMesh' ('(n,x) ': xs) p
    = ( ShaderData x
      , Match (FirType x) (DSetBinding' 2 n p)
              (Text "Mesh binding #" :<>: ShowType n :<>: TypeAndInternalType x
               :<>: Text " isn't compatible with the descriptor binding #" :<>: ShowType n :<>: Text " of type " :<>: ShowType (DSetBinding' 2 n p))
      , CompatibleMesh' xs p
      )

-- | 'CompatibleMaterial' validates the material properties againsts the
-- properties expected in the descriptor set #1 by the shader pipeline.
type CompatibleMaterial bs p
      = ( MatchPropertiesSize bs (DSetBindings 1 p)
                              (Text " material properties in material " :<>: ShowType bs)
                              (Text " descriptors in descriptor set #1.")
        , CompatibleMaterial' (Zip (NumbersFromTo 0 (Length bs)) bs) p
        )
type CompatibleMaterial' :: [(Nat,Type)] -> PipelineInfo -> Constraint
type family CompatibleMaterial' as p where
  CompatibleMaterial' '[] _ = ()
  CompatibleMaterial' ('(n,x) ': xs) p
    = ( ShaderData x
      , Match (FirType x) (DSetBinding' 1 n p)
              (Text "Material binding #" :<>: ShowType n :<>: TypeAndInternalType x
               :<>: Text " isn't compatible with the descriptor binding #" :<>: ShowType n :<>: Text " of type " :<>: ShowType (DSetBinding' 1 n p))
      , CompatibleMaterial' xs p
      )

-- | 'CompatiblePipeline' validates the render pipeline properties againsts the
-- properties expected in the descriptor set #0 by the shader pipeline.
type CompatiblePipeline cs p
      = ( MatchPropertiesSize cs (DSetBindings 0 p)
                              (Text " render properties in render pipeline properties " :<>: ShowType cs)
                              (Text " descriptors in descriptor set #0.")
        , CompatiblePipeline' (Zip (NumbersFromTo 0 (Length cs)) cs) p
        )
type CompatiblePipeline' :: [(Nat,Type)] -> PipelineInfo -> Constraint
type family CompatiblePipeline' as p where
  CompatiblePipeline' '[] p = ()
  CompatiblePipeline' ('(n,x) ': xs) p
    = ( ShaderData x
      , Match (FirType x) (DSetBinding' 0 n p)
              (Text "Render property binding #" :<>: ShowType n :<>: TypeAndInternalType x
                :<>: Text " isn't compatible with the descriptor binding #" :<>: ShowType n :<>: Text " of type " :<>: ShowType (DSetBinding' 0 n p))
      , CompatiblePipeline' xs p
      )

type TypeAndInternalType :: Type -> ErrorMessage
type family TypeAndInternalType x where
  TypeAndInternalType x = Text " of type " :<>: ShowType x :<>: Text " and internal type " :<>: ShowType (FirType x)


------- Matching -----------------------------------

type Match :: k -> k -> ErrorMessage -> Constraint
type family Match a b e where
  Match x x _ = ()
  Match _ _ e = Unsatisfiable e

type MatchPropertiesSize :: [a] -> [b] -> ErrorMessage -> ErrorMessage -> Constraint
type family MatchPropertiesSize l l' e e' where
  MatchPropertiesSize l l' e e' = MatchPropertiesSize' (Length l) (Length l') e e'

type MatchPropertiesSize' :: Nat -> Nat -> ErrorMessage -> ErrorMessage -> Constraint
type family MatchPropertiesSize' t t' e e' where
  MatchPropertiesSize' x x _ _ = ()
  MatchPropertiesSize' x y s1 s2
    = Unsatisfiable (Text "There are " :<>: ShowType x :<>: s1 :<>: Text " but " :<>: ShowType y :<>: s2)


------- Descriptor Set Bindings --------------------

-- To calculate all the bindings in a descriptor set we simply keep trying the next descriptor until there's no more.
type family DSetBindings (set :: Nat) (info :: PipelineInfo) :: [Type] where
  DSetBindings n info = DSetBindings' n info 0

type family DSetBindings' (set :: Nat) (info :: PipelineInfo) (i :: Nat) :: [Type] where
  DSetBindings' n info i = DSetBindings'' n info i (DSetBinding n i info)

type family DSetBindings'' set info i x where
  DSetBindings'' _ _ _ 'Nothing = '[]
  DSetBindings'' n info i ('Just x) = x ': DSetBindings' n info (i+1)

-- | Find descriptor set #set and binding #binding in any of the pipeline stages inputs
type family DSetBinding (set :: Nat) (binding :: Nat) (info :: PipelineInfo) :: Maybe Type where
  DSetBinding set binding (VertexInputInfo _ _ _) = 'Nothing
  DSetBinding set binding (infos `Into` '(_name, 'EntryPointInfo _ defs _)) =
    FindDSetInput set binding (Values (Join (Values defs))) :<|>: DSetBinding set binding infos

type family DSetBinding' (set :: Nat) (binding :: Nat) (info :: PipelineInfo) :: Type where
  DSetBinding' set binding info =
    FromMaybe (DSetBinding set binding info)
              (TypeError (Text "Descriptor [Descriptor Set #" :<>: ShowType set :<>: Text ", Binding #" :<>: ShowType binding :<>: Text "] not found in " :<>: ShowType info))

type family FindDSetInput (set :: Nat) (binding :: Nat) (inputs :: [TLInterfaceVariable]) :: Maybe Type where
  FindDSetInput set binding '[] = 'Nothing
  FindDSetInput set binding ('( '[DescriptorSet set, Binding binding], ty) ': inputs) = 'Just ty
  FindDSetInput set binding ('( '[Binding binding, DescriptorSet set], ty) ': inputs) = 'Just ty
  FindDSetInput set binding (_ ': inputs) = FindDSetInput set binding inputs


------- Inputs -------------------------------------

type InputLocations :: PipelineInfo -> [Nat :-> SPIRV.ImageFormat Nat]
type family InputLocations info where
  InputLocations info = InputLocations' (GetVertexInputInfo info)

type InputLocations' :: (PrimitiveTopology Nat, VertexLocationDescriptions, BindingStrides) -> [Nat :-> SPIRV.ImageFormat Nat]
type family InputLocations' a where
  InputLocations' '(_, vertexLocations, _) = TakeImageFormat vertexLocations

type TakeImageFormat :: VertexLocationDescriptions -> [Nat :-> SPIRV.ImageFormat Nat]
type family TakeImageFormat os where
  TakeImageFormat '[] = '[]
  TakeImageFormat ((n ':-> '(_,_,img)) ': rs) = (n ':-> img) ': TakeImageFormat rs

type InputByLocation' :: Nat -> PipelineInfo -> SPIRV.ImageFormat Nat
type family InputByLocation' loc info where
  InputByLocation' loc info = FromMaybe (Lookup loc (InputLocations info)) (TypeError (Text "Input [Location #" :<>: ShowType loc :<>: Text "] not found in " :<>: ShowType info))

----------------------------------------------------
