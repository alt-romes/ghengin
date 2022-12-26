{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BlockArguments #-}
module Ghengin.Render where

import GHC.Records
import Apecs (Storage, cfold)

import Ghengin.Scene.Graph
import Ghengin.Render.Packet
import Ghengin.Render.Queue
import Ghengin.Component.Transform
import Ghengin.Component.Mesh
import Ghengin.Component.Material
import {-# SOURCE #-} Ghengin (Ghengin)


type RenderConstraints w = ( HasField "meshes" w (Storage Mesh)
                           , HasField "materials" w (Storage SharedMaterial)
                           , HasField "transforms" w (Storage Transform)
                           , HasField "renderPackets" w (Storage RenderPacket)
                           , HasField "modelMatrices" w (Storage ModelMatrix)
                           , HasField "entityParents" w (Storage Parent)
                           )

{-
Note [Renderer]
~~~~~~~~~~~~~~~

The renderer (see 'render') first traverses the scene graph and fills the
render queue, and then traverses the render queue and issues a draw call for
each renderable entity.


 -}

-- | 'render' first traverses the scene graph and fills the render queue, and
-- then traverses the render queue and issues a draw call for each renderable
-- entity.
--
-- Additionally, see notes:
--  * Note [Renderer] (TODO)
--  * Note [Render Queue]
--  * Note [Scene Graph]
--  * Note [Renderable entities] (TODO)
render :: RenderConstraints w
       => Int -- frame identifier
       -> Ghengin w ()
render i = do
  -- TODO: Add to render queue which will sort in render order

  -- Traverse all nodes in the scene graph updating the model matrices
  -- TODO: Currently called traverseSceneGraph, but the name should reflect that the model matrices are updated
  traverseSceneGraph i (const $ pure ())

  -- Fold all 'RenderPacket's into a 'RenderQueue'
  renderPackets <- cfold (\acc (p :: RenderPacket) -> p:acc) mempty

  -- Now, render the renderable entities from the render queue in the given order.
  -- If everything works as expected, if we blindly bind the descriptor sets as
  -- they come, we should bind the pipeline once and each material once.
  traverseRenderQueue (makeRenderQueue renderPackets) \packet -> do

    -- First, bind the pipeline.

