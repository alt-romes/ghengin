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
  (_, V.length -> nExts) <- Vk.enumerateInstanceExtensionProperties Nothing

  putStr "N extensions: " >> print nExts

  withDevice $ \(!d) -> pure ()

  loopUntilClosed win $ do

    pure ()

  putStrLn "Goodbye"

