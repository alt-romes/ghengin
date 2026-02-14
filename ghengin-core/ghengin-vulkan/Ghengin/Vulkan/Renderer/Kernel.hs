{-# LANGUAGE NoImplicitPrelude, LinearTypes, PatternSynonyms, QualifiedDo,
   OverloadedRecordDot, BlockArguments #-}
module Ghengin.Vulkan.Renderer.Kernel where

import Data.Kind
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

import Ghengin.Vulkan.Renderer.Command (CommandM, copyFullBuffer, clearColorImage)
import Ghengin.Vulkan.Renderer.Command.Buffer
import Ghengin.Vulkan.Renderer.ImmediateSubmit
import Ghengin.Vulkan.Renderer.Context
import Ghengin.Vulkan.Renderer.Context.Swapchain
import Ghengin.Vulkan.Renderer.Image

import qualified Vulkan as Vk
import Ghengin.Core.Log
import Ghengin.Core.Type.Utils

import qualified Data.Linear.Alias as Alias
import qualified Unsafe.Linear as Unsafe
import Data.IORef
import Data.Finite as Finite
import Data.Data

type Alias = Alias.Alias Renderer

type RendererEnv :: Nat {-^ Number of frames-in-flight -} -> Type
data RendererEnv (n :: Nat) =
  forall (swpcImgs :: Nat {- number of swapchain images -})
  . KnownNat swpcImgs => RendererEnv
    { vkContext         :: !(VulkanContext WithSwapchain)

    -- | We only need a single depth image, even if we do double buffering in other
    -- places. That's because the depth image is only ever accessed by the GPU and
    -- the GPU can only ever write to a single depth image at a time.
    , depthImage        :: !(VulkanImage WithView)

    -- Synchronization
    -- TODO: Use Mutable vectors? Since we're linear!
    , fences            :: !(V.V n Vk.Fence)
    , presentSemaphores :: !(V.V n Vk.Semaphore)
    , renderSemaphores  :: !(V.V swpcImgs Vk.Semaphore)

    -- Command buffers
    , commandPool       :: !Vk.CommandPool
    , commandBuffers    :: !(V.V n (Maybe (Some CommandBuffer)))

    -- , immediateSubmit :: !ImmediateSubmitCtx
    }

type RendererUrEnv :: Nat {-^ Number of frames-in-flight -} -> Type
data RendererUrEnv (n :: Nat) where
  RendererUrEnv ::
    { logger        :: !Logger
    , frameIndexRef :: !(IORef (Finite n))
    } -> RendererUrEnv n

type FramesInFlight :: Nat
type FramesInFlight = 2

newtype Renderer a = Renderer
  { unRenderer :: Linear.ReaderT (Ur (RendererUrEnv FramesInFlight)) (Linear.StateT (RendererEnv FramesInFlight) System.IO.Linear.IO) a }

deriving instance Data.Linear.Functor Renderer
deriving instance Data.Linear.Applicative Renderer
deriving instance Linear.Functor Renderer
deriving instance Linear.Applicative Renderer
deriving instance Linear.Monad Renderer

instance Linear.MonadIO Renderer where
  liftIO io = Renderer $ ReaderT \(Ur _) -> liftIO io
  {-# INLINE liftIO #-}

instance Linear.MonadFail Renderer where
  fail str = Renderer $ ReaderT \(Ur RendererUrEnv{}) -> liftSystemIO (Prelude.fail str)

instance HasLogger Renderer where
  getLogger = Renderer $ ReaderT \(Ur RendererUrEnv{logger}) -> pure (Ur logger)
  {-# INLINE getLogger #-}
  withLevelUp (Renderer (ReaderT r)) = Renderer $ ReaderT $
    \(Ur RendererUrEnv{logger=Logger l d, ..}) -> r (Ur RendererUrEnv{logger=Logger l (d+1),..})
  {-# INLINE withLevelUp #-}

-- | Make a renderer computation from a linear IO action that linearly uses a
-- 'RendererEnv'
renderer :: (RendererEnv FramesInFlight %1 -> System.IO.Linear.IO (a, RendererEnv FramesInFlight)) %1 -> Renderer a
renderer f = Renderer $ ReaderT \(Ur _) -> StateT f

runRenderer' :: Logger -> RendererEnv FramesInFlight ⊸ Renderer a ⊸ System.IO.Linear.IO (a, RendererEnv FramesInFlight)
runRenderer' logger renv (Renderer rend) = Linear.do
  Ur frameIndexRef <- liftSystemIOU (newIORef (natToFinite (Proxy :: Proxy 0) :: Finite FramesInFlight))
  runStateT (runReaderT rend (Ur (RendererUrEnv{..}))) renv

-- | Get the frame index for the next frame in flight
nextFrameInFlight :: Renderer (Ur (Finite FramesInFlight))
nextFrameInFlight = Linear.do
  Ur frameIndexRef <- Renderer $ asks $ \(Ur env) -> Ur env.frameIndexRef
  Ur frameIndex    <- liftSystemIOU (Data.IORef.readIORef frameIndexRef)
  liftSystemIO $ modifyIORef' frameIndexRef (\currentFrame -> Finite.modulo (getFinite currentFrame Prelude.+ 1))
  return (Ur frameIndex)

withVulkanContext :: (VulkanContext WithSwapchain %1 -> System.IO.Linear.IO (a, VulkanContext WithSwapchain)) %1 -> Renderer a
withVulkanContext f = renderer $ \(RendererEnv{..}) -> f vkContext >>= \case
  (a, d') -> pure (a, RendererEnv{vkContext=d',..})

-- todo: use linear optics.
withDevice :: (Vk.Device %1 -> System.IO.Linear.IO (a, Vk.Device)) %1 -> Renderer a
withDevice f = renderer $ Unsafe.toLinear $ \renv -> f renv.vkContext.device >>= \case (a, _d) -> Unsafe.toLinear (\_ -> pure (a, renv)) _d

-- | Unsafely run a Vulkan action on a linear MonadIO that requires a
-- Vulkan.Device reference as a linear action on 'Renderer'.
-- This action assumes the Vk.Device reference is unchanged! If your action,
-- e.g. frees the reference, Bad Things Will Happen
--
-- Note, this is quite unsafe really, but makes usage of non-linear vulkan much easier
unsafeUseDevice :: (Vk.Device -> Unrestricted.IO b) -> Renderer b
unsafeUseDevice f = renderer $ Unsafe.toLinear $ \renv@(RendererEnv{..}) -> Linear.do
  b <- liftSystemIO $ f vkContext.device
  pure (b, renv)

unsafeWithVulkanContext :: (VulkanContext WithSwapchain -> Unrestricted.IO b) -> Renderer b
unsafeWithVulkanContext f = renderer $ Unsafe.toLinear $ \renv@(RendererEnv{..}) -> Linear.do
  b <- liftSystemIO $ f vkContext
  pure (b, renv)

unsafeGetDevice :: Renderer (Ur Vk.Device)
unsafeGetDevice = renderer $ Unsafe.toLinear $ \renv -> pure (Ur renv.vkContext.device, renv)

-- | Submit a command to the immediate submit command buffer that synchronously
-- submits it to the graphics queue
immediateSubmit :: CommandM System.IO.Linear.IO a ⊸ Renderer a
immediateSubmit cmd = renderer $ \(RendererEnv{..}) -> Linear.do
  undefined cmd
  pure (undefined, RendererEnv{..})
  -- ((dev', imsctx'), x) <- immediateSubmit' vkContext _immediateSubmit cmd
  -- pure (x, RendererEnv{vkContext=dev',_immediateSubmit=imsctx',..})

-- | Run a one-shot command that copies the whole data between two buffers.
-- Returns the two buffers, in the order they were passed to the function
copyBuffer :: Vk.Buffer ⊸ Vk.Buffer ⊸ Vk.DeviceSize -> Renderer (Vk.Buffer, Vk.Buffer)
copyBuffer src dst size = Linear.do
  immediateSubmit $
    copyFullBuffer src dst size

-- | Get the extent of the images in the swapchain?
getRenderExtent :: Renderer (Ur Vk.Extent2D)
getRenderExtent = renderer $ Unsafe.toLinear $ \renv ->
  case renv.vkContext.aSwapchainInfo of
    ASwapchainInfo si -> pure (si.swapchainExtent, renv)

-- | Clears all images in the swapchain to the given color
clearRenderImages :: Float -> Float -> Float -> Float -> Renderer ()
clearRenderImages r g b a = Linear.do
  Ur imgs <- renderer $ Unsafe.toLinear $ \(RendererEnv{..}) -> Linear.do
    Ur (_, imgs) <- liftSystemIOU $ do
      case vkContext.aSwapchainInfo of
        ASwapchainInfo si ->
          Vk.getSwapchainImagesKHR vkContext.device si.swapchain
    pure (Ur imgs, RendererEnv{..})

  immediateSubmit $ consume <$> Data.Linear.forM (Vector.toList imgs) (Unsafe.toLinear \img -> clearColorImage img r g b a)

