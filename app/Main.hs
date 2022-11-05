{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Ghengin.Device as G
import Ghengin.Window as G
import qualified Vulkan as Vk

import qualified Data.Vector as V

main :: IO ()
main = withWindow 800 600 "Vulkan" $ \win -> do

  -- getRequiredInstanceExtensions
  (_, nExts) <- Vk.enumerateInstanceExtensionProperties Nothing

  putStr "Extensions: " >> print nExts

  withDevice $ \(!_d) -> pure ()

  loopUntilClosed win $ pure ()

  putStrLn "Goodbye"

