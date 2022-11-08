{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Ghengin.VulkanEngine as VE
import Ghengin.VulkanEngine.GLFW.Window as G
import qualified Vulkan as Vk

main :: IO ()
main = withVulkanEngine $ \eng -> do

  -- getRequiredInstanceExtensions
  (_, nExts) <- Vk.enumerateInstanceExtensionProperties Nothing

  putStr "Extensions: " >> print nExts

  loopUntilClosed (getWin eng) $ pure ()

  putStrLn "Goodbye"

