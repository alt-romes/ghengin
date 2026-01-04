{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module FIR.Vulkan.Memory
  ( allocateMemory
  ) where

-- base
import Prelude
import Control.Monad
  ( guard )
import Data.Bits
  ( (.&.), testBit )
import Data.Word
  ( Word32 )

-- vector
import qualified Data.Vector as Boxed
  ( Vector )
import qualified Data.Vector as Boxed.Vector
  ( (!?), imapMaybe )

-- vulkan
import qualified Vulkan
import qualified Vulkan as Vulkan.Memory
  ( MemoryRequirements(..) )

-- ghengin-core-indep
import qualified Ghengin.Core.Prelude as Linear

import qualified Unsafe.Linear as Unsafe

-----------------------------------------------------------------------------------------------------

allocateMemory
  :: Linear.MonadIO m
  => Vulkan.PhysicalDevice %1
  -> Vulkan.Device %1
  -> Vulkan.MemoryRequirements
  -> Vulkan.MemoryPropertyFlags
  -> Vulkan.MemoryAllocateFlags
  -> m ( Vulkan.DeviceMemory, Vulkan.PhysicalDevice, Vulkan.Device )
allocateMemory = Unsafe.toLinear2 \physicalDevice device memReqs memFlags allocateFlags -> Linear.do

  devMem <- Linear.liftSystemIO $ do

    Vulkan.PhysicalDeviceMemoryProperties
      { Vulkan.memoryTypes
      } <- Vulkan.getPhysicalDeviceMemoryProperties physicalDevice

    let
      possibleMemoryTypeIndices :: Boxed.Vector Word32
      possibleMemoryTypeIndices = ( `Boxed.Vector.imapMaybe` memoryTypes ) \ i_int memoryType -> do
        let
          i :: Word32
          i = fromIntegral i_int
        guard
          ( testBit
              ( Vulkan.Memory.memoryTypeBits memReqs )
              i_int
          )
        guard ( Vulkan.propertyFlags memoryType .&. memFlags >= memFlags )
        pure i

      memoryTypeIndex :: Word32
      memoryTypeIndex =
        case possibleMemoryTypeIndices Boxed.Vector.!? 0 of
          Nothing ->
            error
              ( "No available memory types with requirements:\n"
              ++ show memReqs
              ++ "\nand with flags:\n"
              ++ show memFlags
              )
          Just i -> i

    let
      allocateFlagsInfo :: Vulkan.MemoryAllocateFlagsInfo
      allocateFlagsInfo =
        Vulkan.MemoryAllocateFlagsInfo
          { Vulkan.flags      = allocateFlags
          , Vulkan.deviceMask = 0
          }
      allocateInfo :: Vulkan.MemoryAllocateInfo '[ Vulkan.MemoryAllocateFlagsInfo ]
      allocateInfo =
        Vulkan.MemoryAllocateInfo
          { Vulkan.next            = ( allocateFlagsInfo, () )
          , Vulkan.allocationSize  = Vulkan.Memory.size memReqs
          , Vulkan.memoryTypeIndex = memoryTypeIndex
          }

    Vulkan.allocateMemory device allocateInfo Nothing

  Linear.return (devMem, physicalDevice, device)
