{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Main where

import Ghengin

-- | The Ocean World with required component storages
data OWorld = OWorld

-- | The game engine parametrized over our ocean world
type Game = Ghengin OWorld

-- | The function run when the game is initialized
ini :: Game ()
ini = do
  pure ()

-- | The function run when the game is closed
end :: () -> Game ()
end () = liftIO $ putStrLn "Goodbye!"

-- | The function run every game loop step.
-- The returned boolean indicates whether to stop the game
update :: () -> DeltaTime -> Game Bool
update () dt = pure False

main :: IO ()
main = ghengin OWorld ini undefined update end

-------------- Ocean ------------------

type Position = Vec3
type Particle = Vertex '[Position]

particles :: Int -> [Particle]
particles n = [ Sin (vec3 x 0 z) | x <- [1..nf]
                                 , z <- [1..nf] ]
  where nf = fromIntegral n :: Float

type ParticleMesh = Mesh '[Position]

oceanMesh :: Int -> Game ParticleMesh
oceanMesh = createMesh . particles

type Color = Vec3
type OceanMaterial = Material '[Color]

oceanMat :: RenderPipeline _ -> Color -> Game OceanMaterial
oceanMat p col = lift $ material (StaticBinding col) p


