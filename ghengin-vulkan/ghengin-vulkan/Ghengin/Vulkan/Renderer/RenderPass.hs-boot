module Ghengin.Vulkan.Renderer.RenderPass where

import qualified Vulkan as Vk
import qualified Data.Vector as Vector

data RenderPass = VulkanRenderPass { _renderPass :: Vk.RenderPass
                                   -- | We bundle framebuffer with the 
                                   -- RenderPass because in rendering we have 
                                   -- a fixed SwapChain so the Framebuffer is
                                   -- differentiated just from the rendering pass.
                                   -- That means that we have to create a
                                   -- framebuffer for all of the images
                                   -- in the swap chain and use the one
                                   -- that corresponds to the retrieved
                                   -- image at drawing time.
                                   , _framebuffers :: Vector.Vector Vk.Framebuffer
                                   }
