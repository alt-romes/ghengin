{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Ghengin where

import GHC.Records

import Control.Monad.Reader

import Apecs

import Ghengin.Vulkan.GLFW.Window
import Ghengin.Vulkan

import Ghengin.Component.Mesh


type Ghengin w a = SystemT w Renderer a


windowLoop :: Ghengin w Bool -> Ghengin w ()
windowLoop action = do
  win <- lift (asks (._vulkanWindow._window))
  loopUntilClosedOr win action


ghengin :: HasField "meshes" w (Storage Mesh)
        => w           -- ^ World
        -> Ghengin w a -- ^ Init
        -> Ghengin w b -- ^ Run every simulation step (currently ignored)
        -> Ghengin w Bool -- ^ Run every game loop? iteration. Bool indicates whether we should exit the gameloop
        -- -> Ghengin w c -- ^ Run every draw step?
        -> Ghengin w c -- ^ Run once the game is quit (for now that is when the window closed)
        -> IO ()
ghengin world initialize _simstep loopstep finalize = runVulkanRenderer . (`runSystem` world) $ do

  _ <- initialize


  windowLoop $ do

    b <- loopstep

    drawFrame

    pure b

  _ <- finalize

  pure ()


drawFrame :: HasField "meshes" w (Storage Mesh) => Ghengin w ()
drawFrame = do

  cmapM_ $ \(Mesh mesh) -> liftIO . print $ mesh

