{-# LANGUAGE NoImplicitPrelude, LinearTypes, PatternSynonyms, QualifiedDo,
   OverloadedRecordDot, BlockArguments #-}
module Ghengin.Vulkan.Renderer.Kernel where

import GHC.TypeNats
import qualified Prelude as Unrestricted
import Prelude.Linear
import qualified Prelude
import qualified System.IO.Linear
import qualified Data.Functor.Linear as Data.Linear
import Control.Functor.Linear as Linear
import Control.Monad.IO.Class.Linear as Linear
import qualified Data.V.Linear as V
import qualified Data.Vector as Vector

import Ghengin.Vulkan.Renderer.Device
import Ghengin.Vulkan.Renderer.Command (CommandM, copyFullBuffer, clearColorImage)
import Ghengin.Vulkan.Renderer.ImmediateSubmit
import Ghengin.Vulkan.Renderer.SwapChain
import Ghengin.Vulkan.Renderer.Frame

-- One day abstract over Window API
import Ghengin.Vulkan.Renderer.GLFW.Window

import qualified Vulkan as Vk
import Ghengin.Core.Log

import qualified Data.Linear.Alias as Alias
import qualified Unsafe.Linear as Unsafe

type Alias = Alias.Alias Renderer

data RendererEnv =
  REnv { _instance        :: !Vk.Instance
       , _vulkanDevice    :: !VulkanDevice
       , _vulkanWindow    :: !VulkanWindow
       , _vulkanSwapChain :: !VulkanSwapChain
       , _commandPool     :: !Vk.CommandPool
       , _frames          :: !(V.V 2 VulkanFrameData)
       -- , _frameInFlight   :: !(IORef Int)
       , _immediateSubmit :: !ImmediateSubmitCtx
       }
newtype RendererReaderEnv
  = RREnv { _logger :: Logger 
          -- ^ Logger and its cleanup action worry about performance later,
          -- correctness first
          }
-- ROMES: Worried linear StateT might reduce performance, hope not
newtype Renderer a
  = Renderer { unRenderer :: Linear.ReaderT (Ur RendererReaderEnv) (Linear.StateT RendererEnv System.IO.Linear.IO) a }

deriving instance Data.Linear.Functor Renderer
deriving instance Data.Linear.Applicative Renderer
deriving instance Linear.Functor Renderer
deriving instance Linear.Applicative Renderer
deriving instance Linear.Monad Renderer

instance Linear.MonadIO Renderer where
  liftIO io = Renderer $ ReaderT \(Ur w) -> StateT \s -> (,s) <$> io
  {-# INLINE liftIO #-}

instance Linear.MonadFail Renderer where
  fail str = Renderer $ ReaderT \(Ur w) -> StateT \s -> (,s) <$> liftSystemIO (Prelude.fail str)

instance HasLogger Renderer where
  getLogger = Renderer $ ReaderT \(Ur w) -> StateT \renv -> pure (Ur w._logger,renv)
  {-# INLINE getLogger #-}
  withLevelUp (Renderer (ReaderT r)) = Renderer $ ReaderT \(Ur RREnv{_logger=Logger l d,..}) -> r (Ur RREnv{_logger=Logger l (d+1),..})
  {-# INLINE withLevelUp #-}

type MAX_FRAMES_IN_FLIGHT_T :: Nat
type MAX_FRAMES_IN_FLIGHT_T = 2 -- We want to work on multiple frames but we don't want the CPU to get too far ahead of the GPU

-- pattern MAX_FRAMES_IN_FLIGHT :: Word32
-- pattern MAX_FRAMES_IN_FLIGHT = 2 

-- | Make a renderer computation from a linear IO action that linearly uses a
-- 'RendererEnv'
renderer :: (RendererEnv %1 -> System.IO.Linear.IO (a, RendererEnv)) %1 -> Renderer a
renderer f = Renderer $ ReaderT \(Ur w) -> StateT f

runRenderer' :: Logger -> RendererEnv ⊸ Renderer a ⊸ System.IO.Linear.IO (a, RendererEnv)
runRenderer' l renv (Renderer rend) = runStateT (runReaderT rend (Ur (RREnv l))) renv

useVulkanDevice :: (VulkanDevice %1 -> System.IO.Linear.IO (a, VulkanDevice)) %1 -> Renderer a
useVulkanDevice f = renderer $ \(REnv{..}) -> f _vulkanDevice >>= \case (a, d') -> pure (a, REnv{_vulkanDevice=d',..})

withDevice :: (Vk.Device %1 -> System.IO.Linear.IO (a, Vk.Device)) %1 -> Renderer a
withDevice f = renderer $ Unsafe.toLinear $ \renv -> f (renv._vulkanDevice._device) >>= \case (a, _d) -> Unsafe.toLinear (\_ -> pure (a, renv)) _d

-- | Unsafely run a Vulkan action on a linear MonadIO that requires a
-- Vulkan.Device reference as a linear action on 'Renderer'.
-- This action assumes the Vk.Device reference is unchanged! If your action,
-- e.g. frees the reference, Bad Things Will Happen
--
-- Note, this is quite unsafe really, but makes usage of non-linear vulkan much easier
unsafeUseDevice :: (Vk.Device -> Unrestricted.IO b) -> Renderer b
unsafeUseDevice f = renderer $ Unsafe.toLinear $ \renv@(REnv{..}) -> Linear.do
  b <- liftSystemIO $ f (_vulkanDevice._device)
  pure $ (b, renv)

unsafeUseVulkanDevice :: (VulkanDevice -> Unrestricted.IO b) -> Renderer b
unsafeUseVulkanDevice f = renderer $ Unsafe.toLinear $ \renv@(REnv{..}) -> Linear.do
  b <- liftSystemIO $ f _vulkanDevice
  pure $ (b, renv)

unsafeGetDevice :: Renderer (Ur Vk.Device)
unsafeGetDevice = renderer $ Unsafe.toLinear $ \renv -> pure (Ur renv._vulkanDevice._device, renv)

-- | Like 'unsafeUseDevice' but additionally use unsafely a linearly value in the unsafe function
unsafeUseDeviceAnd :: (a -> Vk.Device -> Unrestricted.IO b)
                   -> a ⊸ Renderer (b, a)
unsafeUseDeviceAnd f = Unsafe.toLinear $ \x -> (,x) <$> unsafeUseDevice (f x)

-- | Submit a command to the immediate submit command buffer that synchronously
-- submits it to the graphics queue
immediateSubmit :: CommandM System.IO.Linear.IO a ⊸ Renderer a
immediateSubmit cmd = renderer $ \(REnv{..}) -> Linear.do
  ((dev', imsctx'), x) <- immediateSubmit' _vulkanDevice _immediateSubmit cmd
  pure (x, REnv{_vulkanDevice=dev',_immediateSubmit=imsctx',..})

-- | Run a one-shot command that copies the whole data between two buffers.
-- Returns the two buffers, in the order they were passed to the function
copyBuffer :: Vk.Buffer ⊸ Vk.Buffer ⊸ Vk.DeviceSize -> Renderer (Vk.Buffer, Vk.Buffer)
copyBuffer src dst size = Linear.do
  immediateSubmit $
    copyFullBuffer src dst size

-- | Get the extent of the images in the swapchain?
getRenderExtent :: Renderer (Ur Vk.Extent2D)
getRenderExtent = renderer $ Unsafe.toLinear $ \renv -> pure (Ur renv._vulkanSwapChain._surfaceExtent, renv)

-- | Clears all images in the swapchain to the given color
clearRenderImages :: Float -> Float -> Float -> Float -> Renderer ()
clearRenderImages r g b a = Linear.do
  Ur imgs <- renderer $ Unsafe.toLinear $ \(REnv{..}) -> Linear.do
    Ur (_, imgs) <- liftSystemIOU $ Vk.getSwapchainImagesKHR _vulkanDevice._device _vulkanSwapChain._swapchain
    pure (Ur imgs, REnv{..})

  immediateSubmit $ consume <$> (Data.Linear.forM (Vector.toList imgs) (Unsafe.toLinear \img -> clearColorImage img r g b a))

-- Move to utils?
consumeUnits :: V.V n () ⊸ ()
consumeUnits = Unsafe.toLinear \_ -> ()

