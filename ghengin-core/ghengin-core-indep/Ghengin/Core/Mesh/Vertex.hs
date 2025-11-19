{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Ghengin.Core.Mesh.Vertex
  ( Vertex(Sin,(:&),(:&:))
  ) where

import GHC.TypeNats
import Data.Proxy
import Graphics.Gl.Block
import Foreign.Ptr.Diff (Diff(..))
import Foreign.Storable
import Prelude
import Language.Haskell.TH.Syntax

-- for orphan instances while they aren't upstreamed
import Geomancy.Vec3
import Geomancy.Vec2


-- ROMES:TODO: It turns out alignment for Vertices is even something else entirely!!!
-- See FIR.Validation.Layout for information about the layout of vertice
-- information in shaders, slots (locations, components), and references to the
-- specification...
-- https://sheaf.gitlab.io/fir/FIR-Validation-Layout.html
--
-- The vulkan docs for shader layouts:
-- https://docs.vulkan.org/guide/latest/shader_memory_layout.html
--
-- std140 may be very slightly slower (?) if this were put under a lot of stress
-- (though it never will) -- for that case we could use existing vulkan
-- extensions to allow std430 for uniform buffers too (VK_KHR_uniform_buffer_standard_layout)
--
-- So I don't even know if it is possible to provide an instance for a vertex
-- of any attributes. Maybe FIR's Poke can do it...
--
-- Otherwise, we may only allow V4 vertice data and align to that...

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

-- It is kind of precarious to copy over the generic implementation here.
-- Would be better to implement Generic for Vertex, and derive generically Block.
-- May need this https://ryanglscott.github.io/2018/02/11/how-to-derive-generic-for-some-gadts/
-- But failed my attempts because I had to bring into scope type variables in the RHS of the type family and couldn't...

instance Block x => Block (Vertex '[x]) where
  type PackedSize (Vertex '[x]) = PackedSize x
  alignment140 _ = lcm 16 $ alignment140 (Proxy @x)
  alignment430 _ = alignment430 (Proxy @x)
  sizeOf140    _ = roundUp (sizeOf140 (Proxy @x)) (alignment140 (Proxy @x))
  sizeOf430    _ = roundUp (sizeOf430 (Proxy @x)) (alignment430 (Proxy @x))
  sizeOfPacked _ = sizeOfPacked (Proxy @x)
  read140 p (Diff o) = Sin <$> read140 p (Diff o)
  read430 p (Diff o) = Sin <$> read430 p (Diff o)
  readPacked p (Diff o) = Sin <$> readPacked p (Diff o)
  write140 p (Diff o) (Sin a) = write140 p (Diff o) a
  write430 p (Diff o) (Sin a) = write430 p (Diff o) a
  writePacked p (Diff o) (Sin a) = writePacked p (Diff o) a

instance (Block (Vertex (y:ys)), Block x) => Block (Vertex (x:y:ys)) where
  type PackedSize (Vertex (x ': y ': ys)) = PackedSize x + PackedSize (Vertex (y:ys))
  alignment140 _ = max (alignment140 (Proxy @(Vertex '[x]))) (alignment140 (Proxy @(Vertex (y:ys))))
  alignment430 _ = max (alignment430 (Proxy @(Vertex '[x]))) (alignment430 (Proxy @(Vertex (y:ys))))
  sizeOf140    _ = roundUp (sizeOf140 (Proxy @(Vertex '[x]))) (alignment140 (Proxy @(Vertex (y:ys))))
                   + sizeOf140 (Proxy @(Vertex (y:ys)))
  sizeOf430    _ = roundUp (sizeOf430 (Proxy @(Vertex '[x]))) (alignment430 (Proxy @(Vertex (y:ys))))
                   + sizeOf430 (Proxy @(Vertex (y:ys)))
  sizeOfPacked _ = sizeOfPacked (Proxy @(Vertex '[x])) + sizeOfPacked (Proxy @(Vertex (y:ys)))
  read140 p (Diff o) =
    (:&)
      <$> ((\case Sin x -> x) <$> read140 p (Diff @_ @(Vertex '[x]) o))
      <*> read140 p (Diff $ o + roundUp (sizeOf140 (Proxy @(Vertex '[x]))) (alignment140 (Proxy @(Vertex (y:ys)))))
  read430 p (Diff o) =
    (:&)
      <$> ((\case Sin x -> x) <$> read430 p (Diff @_ @(Vertex '[x]) o))
      <*> read430 p (Diff $ o + roundUp (sizeOf430 (Proxy @(Vertex '[x]))) (alignment430 (Proxy @(Vertex (y:ys)))))
  readPacked p (Diff o) =
    (:&)
      <$> ((\case Sin x -> x) <$> readPacked p (Diff @_ @(Vertex '[x]) o))
      <*> readPacked p (Diff $ o + sizeOfPacked (Proxy @(Vertex '[x])))
  write140 p (Diff o) (a :& b) = do
    write140 p (Diff o) (Sin a)
    write140 p (Diff $ o + roundUp (sizeOf140 (Proxy @(Vertex '[x]))) (alignment140 (Proxy @(Vertex (y:ys))))) b
  write430 p (Diff o) (a :& b) = do
    write430 p (Diff o) (Sin a)
    write430 p (Diff $ o + roundUp (sizeOf430 (Proxy @(Vertex '[x]))) (alignment430 (Proxy @(Vertex (y:ys))))) b
  writePacked p (Diff o) (a :& b) = do
    writePacked p (Diff o) (Sin a)
    writePacked p (Diff $ o + sizeOfPacked (Proxy @(Vertex '[x]))) b

-- NB: We use Std140 for Storable Vertex, which isn't quite right since
-- vertices need to abide by the location/component layout, but is fine for now.
-- Note Ghengin.Vulkan.Renderer.Buffer assumes this too.
deriving via (Std140 (Vertex '[x])) instance Block x => Storable (Vertex '[x])
deriving via (Std140 (Vertex (x:y:ys))) instance (Block x, Block (Vertex (y:ys))) => Storable (Vertex (x:y:ys))

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
