{-# OPTIONS_GHC -Wno-orphans #-} -- instance for worlds
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
{-|

Note [Scene Graph]
~~~~~~~~~~~~~~~~~~

A scene graph is a data structure that organizes the renderable elements of a
scene. It serves many different purposes regarding organization and performance
when rendering the scene.

When rendering, we must take into consideration information regarding the
objects being drawn to do it as efficiently as possible:

(1) Discard large parts of the world that are nowhere near the camera before rendering.
(2) Sort the render packets in an order that guarantees the maximum
    number of objects with the same material are rendered sequentially, so we can
    avoid binding and rebinding the same materials (which results in unnecessary
    work/thrashing?)
(3) Possibly sort the objects in a front to back order to avoid work in
    the fragment shader stage since the fragments with the highest depth value will
    be rendered first.
(4) Organizes the transform hierarchy and makes it possible to create a
    model to world matrix from the model transform defined relatively to the
    parent, i.e. taking into account the relative position of each element
    above in the hierarchy


A scene graph's primary purpose is to organize nodes hierarchically, mainly to
solve (4) and pass down information across nodes

It might be possible to an interface to a structure that manages all 4
individually but under the same abstraction?

I was initially confused and somewhat stuck because I was trying to unify all ideas
into a single graph structure, but after reading http://lspiroengine.com/?p=566 I realized it isn't so.

In L Spiro Engine a scene graph is just to manage node hierarchy, which is the
same idea behind most university level explanations of scene graphs.

In Game Engine Architecture, it is said that a scene graph is the structure
that organizes objects spatially and solves (1).

All in all, we might have separate abstractions to solve the different
problems, or we might tie them behind a single abstraction but keep the
implementation simple by composition rather than trying to do all at the same time.

Some more resources:

* Game Engine Architecture
* http://lspiroengine.com/?p=566
* https://teamwisp.github.io/research/scene_graph.html

Our take:
~~~~~~~~~

We're going to implement the scene graph to solve (4) only through the ECS
system by having a parent component in objects not at the root of the scene.

To render, we go through all renderable entities and update a 'WorldMatrix' if
we haven't done it previously in this render sequence before (which we keep
track of with identifiers that change each time we render). Note that the world
matrix isn't computed right away, but rather when it's required because of
laziness. This world matrix is then passed to the draw call?

We provide an abstraction for writing the scene graph as if it were a graph, but internally create the parent components.

Example scene graph:

@
registerSceneGraph :: Ghengin w ()
registerSceneGraph = sceneGraph do
  -- Create two entities at the root level
  newEntity (...)
  newEntity (...)

  -- Create an entity at the root level which has two simple children, and a
  -- child with two children of their own
  newEntity' (...) do
    -- The parent is the entity created above
    newEntity (...)
    newEntity (...)
    newEntity' (...) do
      newEntity (...)
      newEntity (...)

  -- A final top level entity
  newEntity (...)
  ...
@

 -}
module Ghengin.Scene.Graph
  ( sceneGraph
  , newEntity
  , newEntity'
  , Parent(..)
  , ModelMatrix(..)
  , traverseSceneGraph
  , SceneGraph -- ^ Export type but not the definition...
  , EntityConstraints
  ) where

import Prelude
import Data.Unrestricted.Linear (UrT(..))
import Control.Monad.Reader
-- import Prelude.Linear
-- import qualified Prelude
-- import Control.Functor.Linear as Linear hiding (get)
-- import qualified Data.Functor.Linear as Data.Linear
-- import Data.Maybe.Linear

import Geomancy.Mat4

import Data.Maybe
import Apecs (Entity, Set, Get, EntityCounter, Storage, Has, Component, Map, set, get, cmapM)
import Apecs.Core (ExplSet)
import qualified Apecs as Apecs

import {-# SOURCE #-} Ghengin.World
import {-# SOURCE #-} Ghengin (Ghengin)

import Ghengin.Core.Render.Packet
import Ghengin.Vulkan.Renderer.Kernel
import Ghengin.Component.Transform
import Ghengin.Component.Camera
import Ghengin.Component.Orphans

-- ROMES:TODO: How to make the scene-graph an add-on to the Core rendering function

instance Component Parent where
  type Storage Parent = Map Parent

instance Component ModelMatrix where
  type Storage ModelMatrix = Map ModelMatrix

-- TODO: Is having Maybe Entity bad for performance and would perhaps be better to simply have a global parent with an identity transform?
type SceneGraph w = ReaderT (Maybe Entity) (Ghengin w)

-- what if instead of each entity having a parent, we have a global object with
-- a component called "Parents Matrix", which is a binary matrix representing
-- the parenting relationship. Would it be more efficient? How would we use it?

newtype Parent = Parent Entity

-- | The integer identifies the instance in which this model matrix was computed
--
-- It could be important that the matrix field is lazy... (not yet i think)... if we were to try to cache something
data ModelMatrix where
  ModelMatrix :: Mat4 -> {-# UNPACK #-} !Int -> ModelMatrix

sceneGraph :: SceneGraph w a -> Ghengin w a
sceneGraph = flip runReaderT Nothing

type EntityConstraints w c =
  ( Has (World w) (UrT Renderer) EntityCounter
  , Has (World w) (UrT Renderer) Parent
  , Has (World w) (UrT Renderer) ModelMatrix
  , Has (World w) (UrT Renderer) c
  , ExplSet (UrT Renderer) (Storage c)
  , Set (World w) (Ghengin w) c, Get (World w) (Ghengin w) EntityCounter
  )

newEntity :: EntityConstraints w c => c -> SceneGraph w Entity
newEntity c = ask >>= \mparentId -> lift $ do
  ne <- Apecs.newEntity c
  Apecs.set ne (Parent <$> mparentId)
  pure ne

-- | Create an entity with children
newEntity' :: EntityConstraints w c => c -> SceneGraph w a -> SceneGraph w (Entity, a)
newEntity' c sub = ask >>= \mparentId -> do
  ne <- lift $ Apecs.newEntity c
  lift $ Apecs.set ne (Parent <$> mparentId)
  a  <- local (const $ Just ne) sub
  pure (ne, a)

type TraverseConstraints w =
  ( 
    Has (World w) (UrT Renderer) Transform
  , Has (World w) (UrT Renderer) Camera
  , Has (World w) (UrT Renderer) Parent
  , Has (World w) (UrT Renderer) ModelMatrix
  )

{-|
Note [Traversing the scene graph]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The scene graph is a structure implicitly encoded in the entity component
system through the use of a 'Parent' component.

Since the main goal of the scene graph is to structure the hierarchy of
entities and their transforms relative to one another, traversing the scene
graph also entails computing the model matrix for each scene entity.

Therefore, to traverse a scene graph, we go over all renderable entities (See
Note [Renderable entities] in Ghengin.Render) and compute a model matrix.

The model matrix is computed by multiplying the parent's model matrix with our
relative transform. However, we must be careful not to waste resources
computing the same model matrix multiple times. To this effect, the model
matrix is stored separately, and calculating it happens the first time it's
needed but isn't available. The subsequent times the model matrix is required
it will already be computed.

Using the lazily computed value simply leaves a problem: when traversing the
scene graph for a new frame, how do we know whether the value was computed in
this or in the previous frame? If it is outdated we must re-compute it, so the
computed model matrix also carries an integer which is unique in each
traversal. If that integer matches this round's one, then we needn't recompute
the model matrix, otherwise, we must.

Perhaps we could integrate this all into a lazy 'computed' field on transform?...

This seems innefficient and perhaps it would be better to have a separate graph
structure we would synchronize somehow else. However, if we get some sort of
API down we can make progress and at a later time, when it matters, come tune
the scene graph implementation


TODO: Move ModelMatrix out of scene graph and make the traversal simply recurse
on the parents
-}
traverseSceneGraph :: TraverseConstraints w
                   => Int -- ^ The frame instance
                   -> (RenderPacket -> ModelMatrix -> Ghengin w ()) -- ^ A function on renderable entities (by their components)
                   -> Ghengin w ()
traverseSceneGraph inst f = do

  cmapM \(p :: RenderPacket, e :: Entity) -> do
    mmm <- computeModelMatrix inst e
    f p (fromMaybe (ModelMatrix identity 0) mmm)

  -- We also want to compute the model matrix of the camera
  cmapM \(_ :: Camera, e :: Entity) -> do
    computeModelMatrix inst e


-- | Recursively compute the model matrix of an entity and of all transitive parents
computeModelMatrix :: ∀ w
                    . TraverseConstraints w
                   => Int -- ^ The frame instance
                   -> Entity
                   -> Ghengin w (Maybe ModelMatrix)
computeModelMatrix i e =
  get @(World w) @_ @(Maybe ModelMatrix, Maybe Transform, Maybe Parent) e >>= \case
    (Nothing, Nothing, Nothing) -> pure Nothing -- We don't have neither transform nor parent.
    (Just _, Nothing, Nothing)  -> error "We have neither transform nor parent, how can we have a model matrix?"
    (Nothing, Just tr, Nothing) -> do
      -- Our model matrix is our transform matrix because we don't have a parent.
      let mm = ModelMatrix (makeTransform tr) i
      set e mm
      pure (Just mm)
    (jmm@(Just (ModelMatrix _ inst)), Just tr, Nothing) ->
      -- Our model matrix must be updated according to our transform if it is outdated
      if inst == i
         then pure jmm
         else do
           let mm = ModelMatrix (makeTransform tr) i
           set e mm
           pure (Just mm)
    (Nothing, Nothing, Just (Parent p)) -> do
      -- Our model matrix is our parent's
      mm <- computeModelMatrix i p
      set e mm
      pure mm
    (jmm@(Just (ModelMatrix _ inst)), Nothing, Just (Parent p)) ->
      -- Our model matrix is our parent's and must be updated if outdated
      if inst == i
         then pure jmm
         else do
           mm <- computeModelMatrix i p
           set e mm
           pure mm
    (Nothing, Just tr, Just (Parent p)) -> do
      -- Our model matrix is our transform times our parent's (MODEL = TRANSFORM X PARENT_MODEL)
      mm <- computeModelMatrix i p >>= \case
        Nothing ->
          pure (Just $ ModelMatrix (makeTransform tr) i)
        Just (ModelMatrix pmm _) -> -- the parent's model matrix is necessarily up to date given it was just recursively computed
          pure (Just $ ModelMatrix (makeTransform tr <> pmm) i)
      set e mm
      pure mm
    (jmm@(Just (ModelMatrix _ inst)), Just tr, Just (Parent p)) -> do
      -- Our model matrix is our transform times our parent's (MODEL = TRANSFORM X PARENT_MODEL) if not outdated
      if inst == i
         then pure jmm
         else do
           mm <- computeModelMatrix i p >>= \case
             Nothing ->
               pure (Just $ ModelMatrix (makeTransform tr) i)
             Just (ModelMatrix pmm _) ->
               pure (Just $ ModelMatrix (makeTransform tr <> pmm) i)
           set e mm
           pure mm


