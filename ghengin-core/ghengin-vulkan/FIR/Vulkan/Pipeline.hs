{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- | Adapted from sheaf's fir-examples/src/Vulkan/Formats.hs
module FIR.Vulkan.Pipeline where

-- base
import Prelude
import Data.Functor
  ( (<&>) )
import Data.Maybe
  ( fromMaybe )
import GHC.TypeNats
  ( Nat )
import Data.Word ( Word32 )

-- vector
import qualified Data.Vector as Boxed.Vector
  ( fromList )

-- fir
import FIR
  ( Shader(..)
  , PipelineInfo
  , PrimitiveConnectedness(..)
  , PrimitiveTopology(..)
  , (:->)((:->))
  , Known, knownValue
  , GetVertexInputInfo
  , BindingStrides, VertexLocationDescriptions
  , ImageFormat
  )

-- vulkan
import qualified Vulkan
import qualified Vulkan.Zero as Vulkan

import FIR.Vulkan.Formats ( simpleFormat )

topology :: PrimitiveTopology n -> Vulkan.PrimitiveTopology
topology Points                    = Vulkan.PRIMITIVE_TOPOLOGY_POINT_LIST
topology (Line List              ) = Vulkan.PRIMITIVE_TOPOLOGY_LINE_LIST
topology (Line Strip             ) = Vulkan.PRIMITIVE_TOPOLOGY_LINE_STRIP
topology (Line Fan               ) = error "Invalid topology: fan of lines."
topology (Triangle List          ) = Vulkan.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
topology (Triangle Strip         ) = Vulkan.PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP
topology (Triangle Fan           ) = Vulkan.PRIMITIVE_TOPOLOGY_TRIANGLE_FAN
topology (Line AdjacencyList     ) = Vulkan.PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY
topology (Line AdjacencyStrip    ) = Vulkan.PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY
topology (Triangle AdjacencyList ) = Vulkan.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY
topology (Triangle AdjacencyStrip) = Vulkan.PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY
topology (PatchesOfSize         _) = Vulkan.PRIMITIVE_TOPOLOGY_PATCH_LIST

assemblyInfo
  :: PrimitiveTopology n -> Vulkan.PipelineInputAssemblyStateCreateInfo
assemblyInfo primTop =
  Vulkan.PipelineInputAssemblyStateCreateInfo
    { Vulkan.flags                  = Vulkan.zero
    , Vulkan.topology               = topology primTop
    , Vulkan.primitiveRestartEnable = False
    }

tessellationInfo
  :: PrimitiveTopology Word32 -> Maybe ( Vulkan.PipelineTessellationStateCreateInfo '[] )
tessellationInfo (PatchesOfSize pts) = Just $
  Vulkan.PipelineTessellationStateCreateInfo
    { Vulkan.next               = ()
    , Vulkan.flags              = Vulkan.zero
    , Vulkan.patchControlPoints = fromIntegral pts
    }
tessellationInfo _ = Nothing

topologyAndVertexInputStateInfo
  :: forall
      ( info    :: PipelineInfo               )
      ( top     :: PrimitiveTopology Nat      )
      ( descs   :: VertexLocationDescriptions )
      ( strides :: BindingStrides             )
  . ( '(top, descs, strides) ~ GetVertexInputInfo info
    , Known (PrimitiveTopology Nat)    top
    , Known VertexLocationDescriptions descs
    , Known BindingStrides             strides
    )
  => ( PrimitiveTopology Word32, Vulkan.PipelineVertexInputStateCreateInfo '[] )
topologyAndVertexInputStateInfo =
  let
    primTop :: PrimitiveTopology Word32
    primTop = knownValue @top

    bindingStrides :: [ Word32 :-> Word32 ]
    bindingStrides = knownValue @strides

    attributes :: [ Word32 :-> (Word32, Word32, ImageFormat Word32) ]
    attributes = knownValue @descs

    computeVulkanFormat :: ImageFormat Word32 -> Vulkan.Format
    computeVulkanFormat fmt
      = fromMaybe
          ( error $ "Unsupported format " ++ show fmt ++ " used as a vertex input attribute." )
          ( simpleFormat fmt )

    vertexBindingDescriptions :: [ Vulkan.VertexInputBindingDescription ]
    vertexBindingDescriptions =
      bindingStrides <&> \ ( binding :-> stride ) ->
          Vulkan.VertexInputBindingDescription
            { Vulkan.binding   = binding
            , Vulkan.stride    = stride
            , Vulkan.inputRate = Vulkan.VERTEX_INPUT_RATE_VERTEX
            }

    vertexAttributeDescriptions :: [ Vulkan.VertexInputAttributeDescription ]
    vertexAttributeDescriptions =
      attributes <&> \ ( location :-> ( binding, offset, format ) ) ->
        Vulkan.VertexInputAttributeDescription
          { Vulkan.location = location
          , Vulkan.binding  = binding
          , Vulkan.format   = computeVulkanFormat format
          , Vulkan.offset   = offset
          }

    vertexInputStateInfo :: Vulkan.PipelineVertexInputStateCreateInfo '[]
    vertexInputStateInfo =
      Vulkan.PipelineVertexInputStateCreateInfo
        { Vulkan.next                        = ()
        , Vulkan.flags                       = Vulkan.zero
        , Vulkan.vertexBindingDescriptions   = Boxed.Vector.fromList vertexBindingDescriptions
        , Vulkan.vertexAttributeDescriptions = Boxed.Vector.fromList vertexAttributeDescriptions
        }

  in ( primTop, vertexInputStateInfo )


stageFlag :: Shader %m -> Vulkan.ShaderStageFlagBits
stageFlag VertexShader                 = Vulkan.SHADER_STAGE_VERTEX_BIT
stageFlag TessellationControlShader    = Vulkan.SHADER_STAGE_TESSELLATION_CONTROL_BIT
stageFlag TessellationEvaluationShader = Vulkan.SHADER_STAGE_TESSELLATION_EVALUATION_BIT
stageFlag GeometryShader               = Vulkan.SHADER_STAGE_GEOMETRY_BIT
stageFlag FragmentShader               = Vulkan.SHADER_STAGE_FRAGMENT_BIT
stageFlag ComputeShader                = Vulkan.SHADER_STAGE_COMPUTE_BIT
