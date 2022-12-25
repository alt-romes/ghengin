{-# LANGUAGE DataKinds #-}
module Ghengin.Render where

import GHC.Records
import Apecs (Storage)

import Ghengin.Scene.Graph
import Ghengin.Render.Queue
import Ghengin.Component.Transform
import Ghengin.Component.Mesh
import Ghengin.Component.Material
import {-# SOURCE #-} Ghengin (Ghengin)


type RenderConstraints w = ( HasField "meshes" w (Storage Mesh)
                           , HasField "materials" w (Storage SharedMaterial)
                           , HasField "transforms" w (Storage Transform)
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
  traverseSceneGraph i (const $ pure ())

