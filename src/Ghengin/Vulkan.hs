module Ghengin.Vulkan where


rateFn :: Vk.PhysicalDevice -> IO (Maybe Int)
rateFn d = do
  props <- Vk.getPhysicalDeviceProperties d
  feats <- Vk.getPhysicalDeviceFeatures d
  (_, extensionProps) <- Vk.enumerateDeviceExtensionProperties d Nothing

  queueFamilies <- findQueueFamilies d sr
  (SCSD _ forms pmodes) <- querySwapChainSupport d sr

  let
      s1 = if props.deviceType == Vk.PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
             then 1000
             else 0

      s2 = props.limits.maxImageDimension2D

      c1 = isJust queueFamilies

      extensionsSupported = not . null $ L.intersect (V.toList deviceExtensions) (V.toList $ V.map (.extensionName) extensionProps)

      swapChainAdequate = not (null forms) && not (null pmodes)

     -- If the app couldn't function without geometry shaders
      _c8 = feats.geometryShader

  if c1 && extensionsSupported && swapChainAdequate
     then pure (s1 + fromIntegral s2, d)
     else pure (0, d)


