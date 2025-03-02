{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Ghengin.Core.Mesh
  ( Mesh(..) -- Export these from an Internals module, not from here
  , Some(..)
  , createMesh
  , createMeshWithIxs
  -- , calculateFlatNormals
  -- , calculateSmoothNormals
  , freeMesh

  , meshId

  -- * Vertices
  , module Ghengin.Core.Mesh.Vertex
  ) where

import GHC.Stack
import Ghengin.Core.Prelude as Linear
import Data.Unique

import Data.V.Linear (make)

import Data.Vector.Storable (Storable)
import qualified Data.Vector.Storable as SV

import Ghengin.Core.Mesh.Vertex
import Ghengin.Core.Render.Property
import Ghengin.Core.Type.Compatible ( CompatibleVertex, CompatibleMesh )
import Ghengin.Core.Render.Pipeline ( RenderPipeline(..) )

import Ghengin.Core.Renderer.Kernel
import Ghengin.Core.Renderer.Buffer
import Ghengin.Core.Renderer.DescriptorSet

import Ghengin.Core.Log
import Ghengin.Core.Type.Utils (Some(..))

import qualified Data.Linear.Alias as Alias

{-
Note [Meshes]
~~~~~~~~~~~~~

OUTDATED!

All meshes are freed when the window is closed. However, if you change/discard a
mesh during the game you must free it explicitly. TODO: Enforce it somehow: linear types+reference counting

 -}

type Mesh :: [Type] -- ^ Vertex attributes
          -> [Type] -- ^ Mesh properties
          -> Type
data Mesh ts props where
  SimpleMesh :: !VertexBuffer -- ^ vertexBuffer, a vector of vertices in buffer format
                -- ^ We don't need to keep the Vector Vertex that was originally (unless we wanted to regenerate it every time?)
                -- used to create this Mesh, bc having the vertex buffer and
                -- the device memory is morally equivalent
                -- , vertices :: Vector Vertex
              ⊸ (Alias DescriptorSet, Alias ResourceMap) -- ^ Descriptors for property bindings
              ⊸ !Unique
             -> Mesh ts '[]
  IndexedMesh :: !VertexBuffer -- ^ vertexBuffer, a vector of vertices in buffer format
               ⊸ !Index32Buffer
               ⊸ (Alias DescriptorSet, Alias ResourceMap) -- ^ Descriptors for property bindings
               ⊸ !Unique
              -> Mesh ts '[]
  MeshProperty :: forall p vs ps
                . PropertyBinding p
                ⊸ Mesh vs ps
                ⊸ Mesh vs (p:ps)

meshId :: Mesh ts props ⊸ (Ur Unique, Mesh ts props)
meshId = \case
  MeshProperty p xs -> case meshId xs of
    (uq, xs') -> (uq, MeshProperty p xs')
  SimpleMesh vb ds uq -> (Ur uq, SimpleMesh vb ds uq)
  IndexedMesh vb ib ds uq -> (Ur uq, IndexedMesh vb ib ds uq)

instance HasProperties (Mesh vs) where
  properties :: Mesh vs ps ⊸ Renderer (PropertyBindings ps, Mesh vs ps)
  properties = \case
    MeshProperty p0 xs -> Linear.do
      (p1, p2) <- Alias.share p0
      (xs', mesh') <- properties xs
      pure (p1 :## xs', MeshProperty p2 mesh')
    SimpleMesh a b c -> pure (GHNil, SimpleMesh a b c)
    IndexedMesh a b c d -> pure (GHNil, IndexedMesh a b c d)

  descriptors = \case
    SimpleMesh vb (ds0, rm0) uq -> Linear.do
      (ds1, ds2) <- Alias.share ds0
      (rm1, rm2) <- Alias.share rm0
      pure (ds1, rm1, SimpleMesh vb (ds2, rm2) uq)
    IndexedMesh vb ib (ds0, rm0) uq -> Linear.do
      (ds1, ds2) <- Alias.share ds0
      (rm1, rm2) <- Alias.share rm0
      pure (ds1, rm1, IndexedMesh vb ib (ds2, rm2) uq)
    MeshProperty p xs -> Linear.do
      (dset, rmap, mesh') <- descriptors xs
      pure (dset, rmap, MeshProperty p mesh')

  puncons (MeshProperty p xs) = (p, xs)
  pcons = MeshProperty

      -- TODO: Various kinds of meshes: indexed meshes, strip meshes, just triangles...

-- | Create a 'Mesh' given a list of 'Vertex's, where each 'Vertex' has a set
-- of properties of types @ts@ (e.g. a Vec3 for position and a Vec3 for color).
--
-- The 'Mesh' must be compatible with the given 'RenderPipeline', and should only
-- be rendered using the same render pipeline (in a render queue, this means
-- you should only 'insertMesh' onto a material group that is under this same
-- render pipeline)
--
--
-- TBH, I'm not sure what happens if you somehow try to render the mesh under a
-- different graphics pipeline. But when the mesh is created, we allocate a
-- descriptor set for the mesh from the descriptor set pool associated with
-- this pipeline, so they are at least that tied.
createMesh :: (CompatibleMesh props π, CompatibleVertex ts π, Storable (Vertex ts))
           => RenderPipeline π bs
            -- ^ The render pipeline
            ⊸ PropertyBindings props
            -- ^ The 'PropertyBindings' for the properties of this mesh (the second type argument to 'Mesh')
            ⊸ [Vertex ts]
            -- ^ Vertices
           -> Renderer (Mesh ts props, RenderPipeline π bs)
createMesh (RenderProperty pr rps) props0 vs = createMesh rps props0 vs >>= \case (m, rp) -> pure (m, RenderProperty pr rp)
createMesh (RenderPipeline gpip rpass (rdset, rres, dpool0) shaders) props0 (SV.fromList -> vs) = enterD "createMesh" Linear.do
  Ur uniq      <- liftSystemIOU newUnique
  vertexBuffer <- createVertexBuffer vs

  (dset0, rmap0, dpool1, props1) <- allocateDescriptorsForMeshes dpool0 props0

  pure ( mkMesh (SimpleMesh vertexBuffer (dset0, rmap0) uniq) props1
       , RenderPipeline gpip rpass (rdset, rres, dpool1) shaders
       )

-- | Like 'createMesh', but create the mesh using a vertex buffer created from
-- the vertices and an indexbuffer created from the indices
createMeshWithIxs :: HasCallStack => (CompatibleMesh props π, CompatibleVertex ts π, Storable (Vertex ts))
                  => RenderPipeline π bs
                   ⊸ PropertyBindings props
                   ⊸ [Vertex ts]
                  -- ^ Vertices
                  -> [Int32]
                  -- ^ Indices
                  -> Renderer (Mesh ts props, RenderPipeline π bs)
createMeshWithIxs (RenderProperty pr rps) props0 vs ixs = createMeshWithIxs rps props0 vs ixs >>= \case (m, rp) -> pure (m, RenderProperty pr rp)
createMeshWithIxs (RenderPipeline gpip rpass (rdset, rres, dpool0) shaders) props0 (SV.fromList -> vertices) (SV.fromList -> ixs) = enterD "createMeshWithIxs" Linear.do
  Ur uniq      <- liftSystemIOU newUnique
  vertexBuffer <- createVertexBuffer vertices
  indexBuffer  <- createIndex32Buffer ixs

  (dset0, rmap0, dpool1, props1) <- allocateDescriptorsForMeshes dpool0 props0

  pure ( mkMesh (IndexedMesh vertexBuffer indexBuffer (dset0, rmap0) uniq) props1
       , RenderPipeline gpip rpass (rdset, rres, dpool1) shaders
       )

mkMesh :: ∀ t b. Mesh t '[] ⊸ PropertyBindings b ⊸ Mesh t b
mkMesh x GHNil = x
mkMesh x (p :## pl) = MeshProperty p (mkMesh x pl)

allocateDescriptorsForMeshes :: Alias DescriptorPool ⊸ PropertyBindings props ⊸ Renderer (Alias DescriptorSet, Alias ResourceMap, Alias DescriptorPool, PropertyBindings props)
allocateDescriptorsForMeshes dpool0 props0 = Linear.do
  -- Mostly just the same as in 'material' in Ghengin.Core.Material

  logT "Allocating descriptor set"
  -- We allocate an empty descriptor set of type #2 to later write with the resource map
  (dpool1,dset0)  <- Alias.useM dpool0 (\pool -> swap <$> allocateEmptyDescriptorSet 2 pool)
  (dpool2,dpool3) <- Alias.share dpool1

  logT "Allocating resource map"
  -- Make the resource map for this material
  (resources0, props1) <- makeResources props0

  logT "Updating descriptor with resources"
  -- Create the descriptor set with the written descriptors based on the created resource map
  (dset1, resources1) <- updateDescriptorSet dset0 resources0

  logT "Making aliases for dset and rmap"
  dset2 <- Alias.newAlias (freeDescriptorSets dpool2 . make) dset1
  resources2 <- Alias.newAlias freeResourceMap resources1

  pure (dset2, resources2, dpool3, props1)

freeMesh :: Mesh vs ps ⊸ Renderer ()
freeMesh mesh = Linear.do
  logD "Freeing mesh..."
  case mesh of
    SimpleMesh (VertexBuffer vb _) ds _ ->
      Alias.forget ds >> destroyDeviceLocalBuffer vb
    IndexedMesh (VertexBuffer vb _) (Index32Buffer ib _) ds _ ->
      Alias.forget ds >> destroyDeviceLocalBuffer vb >> destroyDeviceLocalBuffer ib
    MeshProperty prop xs ->
      Alias.forget prop >> freeMesh xs

-- TODO: Nub vertices (make indexes pointing at different vertices which are equal to point at the same vertice and remove the other)?
