{-# OPTIONS_GHC -ddump-splices     #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Main.Apecs where

import Ghengin.Component.Mesh
import Ghengin.Component.Transform
import Ghengin.Vulkan
import Geomancy hiding (Transform)
import Apecs.TH
import Apecs

newtype Position = Position Vec2 deriving Show
newtype Velocity = Velocity Vec2 deriving Show
data Flying = Flying
makeMapComponents [''Position, ''Velocity, ''Flying]

data World = World { positions :: !(Storage Position)
                   , velocities :: !(Storage Velocity)
                   , flying     :: !(Storage Flying)
                   , meshes     :: !(Storage Mesh)
                   , transforms :: !(Storage Transform)
                   , entityCounter :: !(Storage EntityCounter)
                   }

instance Has World Renderer Position where getStore = SystemT (asks positions)
instance Has World Renderer Velocity where getStore = SystemT (asks velocities)
instance Has World Renderer Flying   where getStore = SystemT (asks flying)
instance Has World Renderer EntityCounter where getStore = SystemT (asks entityCounter)

initWorld :: IO World
initWorld = World <$> explInit <*> explInit <*> explInit <*> explInit <*> explInit <*> explInit

-- makeWorldAndComponents "World" [''Position, ''Velocity, ''Flying]

-- Is the SystemT over Renderer fast enough? Probable yes, since Renderer is just ReaderT over IO. As long as it optimizes...

game :: SystemT World Renderer ()
game = do
  m1 <- lift $ createMesh [Vertex (vec3 0.0 (-0.5) 1) (vec3 0 0 0) (vec3 1 0 0), Vertex (vec3 0.5 0.5 1) (vec3 0 0 0) (vec3 0 1 0), Vertex (vec3 (-0.5) 0.5 1) (vec3 0 0 0) (vec3 0 0 1)]
  newEntity (Position 0, Velocity 1, m1 :: Mesh)
  newEntity (Position 2, Velocity 1)
  newEntity (Position 1, Velocity 2, Flying)

  -- 1. Add velocity to position
  -- 2. Apply gravity to non-flying entities
  -- 3. Print a list of entities and their positions
  cmap $ \(Position p, Velocity v) -> Position (v+p)
  cmap $ \(Velocity v, _ :: Not Flying) -> Velocity (v - vec2 0 1)
  cmap $ \(Position p, Velocity v) -> Position (v+p)
  cmapM_ $ \(Position p, Entity e) -> liftIO . print $ (e, p)



-- simulationSystem :: DeltaTime -> SystemT Renderer
-- updateSystem     :: DeltaTime -> SystemT Renderer
-- runGame simulationSystem updateSystem

-- independent renderer that exports a system that renders all meshes+materials
-- ghengin itself uses the renderer system at the end of the game loop
-- and other physics systems and AI etc

