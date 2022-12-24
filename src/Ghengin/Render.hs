module Ghengin.Render where

import Ghengin.Scene.Graph
import Ghengin.Render.Queue

{-
Note [Renderer]
~~~~~~~~~~~~~~~

The renderer (see 'render') first traverses the scene graph and fills the
render queue, and then traverses the render queue and issues a draw call for
each renderable entity.



Note [Renderable entities]
~~~~~~~~~~~~~~~~~~~~~~~~~~

A renderable entity is any entity with a 'Mesh', a 'Material' component...

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
-- render :: Ghengin w ()
-- render = do
--   traverseSceneGraph ()

