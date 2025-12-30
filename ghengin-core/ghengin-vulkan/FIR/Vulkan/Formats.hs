{-# LANGUAGE PatternSynonyms #-}

-- | Adapted from sheaf's fir-examples/src/Vulkan/Formats.hs
module FIR.Vulkan.Formats where

-- base
import Prelude
import Data.Word
  ( Word32 )

-- vulkan
import qualified Vulkan

-- fir
import FIR
  ( ImageFormat(ImageFormat), pattern UI, pattern I, pattern F )

-------------------------------------------------------------------------

simpleFormat :: ImageFormat Word32 -> Maybe Vulkan.Format
simpleFormat ( ImageFormat UI widths ) = case widths of
  [8]           -> Just Vulkan.FORMAT_R8_UINT
  [8,8]         -> Just Vulkan.FORMAT_R8G8_UINT
  [8,8,8]       -> Just Vulkan.FORMAT_R8G8B8_UINT
  [8,8,8,8]     -> Just Vulkan.FORMAT_R8G8B8A8_UINT
  [16]          -> Just Vulkan.FORMAT_R16_UINT
  [16,16]       -> Just Vulkan.FORMAT_R16G16_UINT
  [16,16,16]    -> Just Vulkan.FORMAT_R16G16B16_UINT
  [16,16,16,16] -> Just Vulkan.FORMAT_R16G16B16A16_UINT
  [32]          -> Just Vulkan.FORMAT_R32_UINT
  [32,32]       -> Just Vulkan.FORMAT_R32G32_UINT
  [32,32,32]    -> Just Vulkan.FORMAT_R32G32B32_UINT
  [32,32,32,32] -> Just Vulkan.FORMAT_R32G32B32A32_UINT
  [64]          -> Just Vulkan.FORMAT_R64_UINT
  [64,64]       -> Just Vulkan.FORMAT_R64G64_UINT
  [64,64,64]    -> Just Vulkan.FORMAT_R64G64B64_UINT
  [64,64,64,64] -> Just Vulkan.FORMAT_R64G64B64A64_UINT
  _             -> Nothing
simpleFormat ( ImageFormat I widths ) = case widths of
  [8]           -> Just Vulkan.FORMAT_R8_SINT
  [8,8]         -> Just Vulkan.FORMAT_R8G8_SINT
  [8,8,8]       -> Just Vulkan.FORMAT_R8G8B8_SINT
  [8,8,8,8]     -> Just Vulkan.FORMAT_R8G8B8A8_SINT
  [16]          -> Just Vulkan.FORMAT_R16_SINT
  [16,16]       -> Just Vulkan.FORMAT_R16G16_SINT
  [16,16,16]    -> Just Vulkan.FORMAT_R16G16B16_SINT
  [16,16,16,16] -> Just Vulkan.FORMAT_R16G16B16A16_SINT
  [32]          -> Just Vulkan.FORMAT_R32_SINT
  [32,32]       -> Just Vulkan.FORMAT_R32G32_SINT
  [32,32,32]    -> Just Vulkan.FORMAT_R32G32B32_SINT
  [32,32,32,32] -> Just Vulkan.FORMAT_R32G32B32A32_SINT
  [64]          -> Just Vulkan.FORMAT_R64_SINT
  [64,64]       -> Just Vulkan.FORMAT_R64G64_SINT
  [64,64,64]    -> Just Vulkan.FORMAT_R64G64B64_SINT
  [64,64,64,64] -> Just Vulkan.FORMAT_R64G64B64A64_SINT
  _             -> Nothing
simpleFormat ( ImageFormat F widths ) = case widths of
  [16]          -> Just Vulkan.FORMAT_R16_SFLOAT
  [16,16]       -> Just Vulkan.FORMAT_R16G16_SFLOAT
  [16,16,16]    -> Just Vulkan.FORMAT_R16G16B16_SFLOAT
  [16,16,16,16] -> Just Vulkan.FORMAT_R16G16B16A16_SFLOAT
  [32]          -> Just Vulkan.FORMAT_R32_SFLOAT
  [32,32]       -> Just Vulkan.FORMAT_R32G32_SFLOAT
  [32,32,32]    -> Just Vulkan.FORMAT_R32G32B32_SFLOAT
  [32,32,32,32] -> Just Vulkan.FORMAT_R32G32B32A32_SFLOAT
  [64]          -> Just Vulkan.FORMAT_R64_SFLOAT
  [64,64]       -> Just Vulkan.FORMAT_R64G64_SFLOAT
  [64,64,64]    -> Just Vulkan.FORMAT_R64G64B64_SFLOAT
  [64,64,64,64] -> Just Vulkan.FORMAT_R64G64B64A64_SFLOAT
  _             -> Nothing
simpleFormat _ = Nothing
