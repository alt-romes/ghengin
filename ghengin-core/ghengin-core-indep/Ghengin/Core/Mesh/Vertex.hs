{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Ghengin.Core.Mesh.Vertex
  ( Vertex(Sin,(:&),(:&:))
  ) where

import Data.Proxy
import Graphics.Gl.Block
import Foreign.Ptr.Diff (Diff(..))
import Foreign.Storable
import Prelude
import Language.Haskell.TH.Syntax

-- for orphan instances while they aren't upstreamed
import Geomancy.Vec3
import Geomancy.Vec2

-- | Each vertex in a mesh is composed of a list of attributes (given as a
-- type-level list of types @vs@).
--
-- Every type in @vs@ will occupy exactly one location. We don't currently
-- support any way to specify two types pertaining to the same location but
-- with different components. For instance, a @Vertex [Vec3, Float]@ will place
-- a @Vec3@ in @Location 0@ and a @Float@ in @Location 1@. We validate that
-- this is what the shader expects using @CompatibleMesh@. Each location is
-- aligned to 16 bytes.
--
-- If your shader wants to have a slightly more complex layout, for example:
--
-- @
-- type VertexData =
--  '[ Slot 0 0 ':-> V 3 Float -- in pos
--   , Slot 0 3 ':-> Float -- uv y position
--   , Slot 1 0 ':-> V 3 Float -- in normal
--   ]
-- @
--
-- You can still construct a vertex which will match that layout by using
-- @Vertex [Vec4, Vec3]@ and filling in the last component of the @Vec4@ with
-- the float.
--
-- More complicated layouts (e.g. with Array, starting at a @Component@ other
-- than 0, etc...) are currently not supported.
--
-- See FIR.Validation.Layout for more information about the layout of vertice
-- information in shaders, slots (locations, components), and references to the
-- specification:
-- - https://sheaf.gitlab.io/fir/FIR-Validation-Layout.html
-- - https://developer.apple.com/library/archive/documentation/3DDrawing/Conceptual/OpenGLES_ProgrammingGuide/TechniquesforWorkingwithVertexData/TechniquesforWorkingwithVertexData.html
data Vertex vs where

  -- | A vertex with a single property
  Sin :: v -> Vertex '[v]

  -- | Add a property to a vertex
  (:&) :: v -> Vertex (v':vs) -> Vertex (v:v':vs)

infixr 6 :&

-- | A vertex with two properties
pattern (:&:) :: v -> v' -> Vertex '[v, v']
pattern (:&:) a b = a :& Sin b
infixr 6 :&:
{-# COMPLETE (:&:) #-}

-- | Serializes the Vertex according to the location/component layout as per the haddocks of @'Vertex'@
instance Block x => Storable (Vertex '[x]) where
  sizeOf    _ = 16
  alignment _ = 16
  peekByteOff p o         = Sin <$> readPacked p (Diff o)
  pokeByteOff p o (Sin v) = writePacked p (Diff o) v

-- | Serializes the Vertex according to the location/component layout as per the haddocks of @'Vertex'@
instance (Storable (Vertex (y:ys)), Block x) => Storable (Vertex (x:y:ys)) where
  sizeOf    _ = 16 + sizeOf (undefined :: Vertex (y:ys))
  alignment _ = 16
  peekByteOff p o =
    (:&) <$> readPacked p (Diff o)
         <*> peekByteOff p (o + roundUp (sizeOfPacked (Proxy :: Proxy x)) 16)
  pokeByteOff p o (v :& vs) = do
    writePacked p (Diff o) v
    pokeByteOff p (o + roundUp (sizeOfPacked (Proxy :: Proxy x)) 16) vs

instance Show x => Show (Vertex '[x]) where
  show (Sin x) = show x
  {-# INLINE show #-}

instance (Show (Vertex (y : xs)), Show x) => Show (Vertex (x : y : xs)) where
  show (x :& xs) = show x <> " :& " <> show xs
  {-# INLINE show #-}

instance Lift x => Lift (Vertex '[x]) where
    lift (Sin x) = [| Sin $(lift x) |]
    liftTyped (Sin x) = [|| Sin $$(liftTyped x) ||]

instance (Lift (Vertex (y : xs)), Lift x) => Lift (Vertex (x : y : xs)) where
    lift (x :& xs) = [| $(lift x) :& $(lift xs) |]
    liftTyped (x :& xs) = [|| $$(liftTyped x) :& $$(liftTyped xs) ||]

--------------------------------------------------------------------------------
-- Orphans
--------------------------------------------------------------------------------

instance Lift Vec3 where
    lift (WithVec3 a b c) = [| vec3 $(lift a) $(lift b) $(lift c) |]
    liftTyped (WithVec3 a b c) = [|| vec3 $$(liftTyped a) $$(liftTyped b) $$(liftTyped c) ||]

instance Lift Vec2 where
    lift (WithVec2 a b) = [| vec2 $(lift a) $(lift b) |]
    liftTyped (WithVec2 a b) = [|| vec2 $$(liftTyped a) $$(liftTyped b) ||]
