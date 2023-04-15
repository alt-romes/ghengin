{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-|
   TODO: Document the 'Compatible' type constraint
 -}
module Ghengin.Core.Type.Compatible
  ( Compatible
  , CompatibleVertex'
  , CompatibleMaterial'
  , CompatibleRender'
  ) where

import Data.Kind ( Type, Constraint )
import Data.Type.List ( Join, Length )
import Data.Type.Maybe ( FromMaybe )
import Data.Type.Map (Values, (:->)(..), Lookup)
import FIR.Pipeline
    ( BindingStrides,
      PipelineInfo(..),
      PrimitiveTopology,
      VertexLocationDescriptions,
      GetVertexInputInfo )
import FIR (Syntactic, InternalType)
import FIR.ProgramState
    ( EntryPointInfo(EntryPointInfo), TLInterfaceVariable )
import GHC.TypeLits
    ( TypeError, type (+), Nat, ErrorMessage((:<>:), ShowType, Text) )
import SPIRV.Decoration (Decoration(..))
import qualified SPIRV.Image as SPIRV
-- TODO: Remove dependency on Ghengin non-core

import Ghengin.Core.Type.Utils

{-
Note [The Compatible constraint]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

TODO
-}

------- Compatible ---------------------------------

-- | 'Compatible' validates at the type level that the mesh and material are
-- compatible with the render pipeline. See Note [Pipeline compatible materials].
type Compatible :: [Type]       -- ^ Vertex properties
                -> [Type]       -- ^ Material properties
                -> [Type]       -- ^ Render properties
                -> PipelineInfo -- ^ The pipeline against which the properties must be compatible
                -> Constraint
type family Compatible αs βs ξs π where
  Compatible as bs cs p
    = ( MatchPropertiesSize (Length as) (Length (InputLocations p))
                            (Text " mesh vertex properties in vertex " :<>: ShowType as)
                            (Text " inputs in the vertex shader.")
      , MatchPropertiesSize (Length bs) (Length (DSetBindings 1 p))
                            (Text " material properties in material " :<>: ShowType bs)
                            (Text " descriptors in descriptor set #1.")
      , MatchPropertiesSize (Length cs) (Length (DSetBindings 0 p))
                            (Text " render properties in render pipeline properties " :<>: ShowType cs)
                            (Text " descriptors in descriptor set #0.")
      , CompatibleVertex'   as p
      , CompatibleMaterial' bs p
      , CompatibleRender'   cs p
      )


type CompatibleVertex' as p = CompatibleVertex (Zip (NumbersFromTo 0 (Length as)) as) p
type CompatibleVertex :: [(Nat,Type)] -> PipelineInfo -> Constraint
type family CompatibleVertex as p where
  CompatibleVertex '[] _ = ()
  CompatibleVertex ('(n,x) ': xs) p
    = ( Syntactic x
      , Match (SizeOf (InternalType x)) (SizeOf (InputByLocation' n p))
                        (Text "Vertex property #" :<>: ShowType n :<>: TypeAndInternalType x
                          :<>: Text " whose internal type is " :<>: ShowType (InternalType x)
                          :<>: Text " isn't compatible with the shader vertex property #" :<>: ShowType n :<>: Text " of type " :<>: ShowType (InputByLocation' n p))
      , CompatibleVertex xs p
      )

type CompatibleMaterial' bs p = CompatibleMaterial (Zip (NumbersFromTo 0 (Length bs)) bs) p
type CompatibleMaterial :: [(Nat,Type)] -> PipelineInfo -> Constraint
type family CompatibleMaterial as p where
  CompatibleMaterial '[] _ = ()
  CompatibleMaterial ('(n,x) ': xs) p
    = ( Syntactic x
      , Match (InternalType x) (DSetBinding' 1 n p)
              (Text "Material binding #" :<>: ShowType n :<>: TypeAndInternalType x
               :<>: Text " isn't compatible with the descriptor binding #" :<>: ShowType n :<>: Text " of type " :<>: ShowType (DSetBinding' 1 n p))
      , CompatibleMaterial xs p
      )

type CompatibleRender' cs p = CompatibleRender (Zip (NumbersFromTo 0 (Length cs)) cs) p
type CompatibleRender :: [(Nat,Type)] -> PipelineInfo -> Constraint
type family CompatibleRender as p where
  CompatibleRender '[] p = ()
  CompatibleRender ('(n,x) ': xs) p
    = ( Syntactic x
      , Match (InternalType x) (DSetBinding' 0 n p)
              (Text "Render property binding #" :<>: ShowType n :<>: TypeAndInternalType x
                :<>: Text " isn't compatible with the descriptor binding #" :<>: ShowType n :<>: Text " of type " :<>: ShowType (DSetBinding' 0 n p))
      , CompatibleRender xs p
      )

type TypeAndInternalType :: Type -> ErrorMessage
type family TypeAndInternalType x where
  TypeAndInternalType x = Text " of type " :<>: ShowType x :<>: Text " and internal type " :<>: ShowType (InternalType x)


------- Matching -----------------------------------

type Match :: k -> k -> ErrorMessage -> Constraint
type family Match a b e where
  Match x x _ = ()
  Match _ _ e = TypeError e

type MatchPropertiesSize :: Nat -> Nat -> ErrorMessage -> ErrorMessage -> Constraint
type family MatchPropertiesSize t t' e e' where
  MatchPropertiesSize x x _ _ = ()
  MatchPropertiesSize x y s1 s2
    = TypeError (Text "There are " :<>: ShowType x :<>: s1 :<>: Text " but " :<>: ShowType y :<>: s2)


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
--
-- TODO: This assumes this order as the only valid one. At least say it so in the error message.
type family DSetBinding (set :: Nat) (binding :: Nat) (info :: PipelineInfo) :: Maybe Type where
  DSetBinding set binding (VertexInputInfo _ _ _) = 'Nothing
  DSetBinding set binding (infos `Into` '(_name, 'EntryPointInfo _ defs _)) =
    FindDSetInput set binding (Values (Join (Values defs))) :<|>: DSetBinding set binding infos

type family DSetBinding' (set :: Nat) (binding :: Nat) (info :: PipelineInfo) :: Type where
  DSetBinding' set binding info =
    FromMaybe (DSetBinding set binding info)
              (TypeError (Text "Uniform [Descriptor Set #" :<>: ShowType set :<>: Text ", Binding #" :<>: ShowType binding :<>: Text "] not found in " :<>: ShowType info))

type family FindDSetInput (set :: Nat) (binding :: Nat) (inputs :: [TLInterfaceVariable]) :: Maybe Type where
  FindDSetInput set binding '[] = 'Nothing
  FindDSetInput set binding ('( '[DescriptorSet set, Binding binding], ty) ': inputs) = 'Just ty
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

