{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE NamedWildCards        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module Shaders where

-- fir
import FIR
import FIR.Syntax.Labels
import Math.Linear

-- ghengin
import qualified Ghengin.Core.Shader as G
import Ghengin.Camera.Shader.Lighting

-- See FIR.Validation.Layout about locations/component layouts...
type VertexInput
  = '[ Slot 0 0 ':-> V 3 Float
     , Slot 1 0 ':-> V 3 Float ]

shaderPipeline :: G.ShaderPipeline _
shaderPipeline
  = G.ShaderPipeline (StructInput @VertexInput @(Triangle List))
  G.:>-> vertex
  G.:>-> fragment

type VertexDefs =
  '[ "in_position"  ':-> Input      '[ Location 0 ] (V 3 Float)
   , "in_normal"    ':-> Input      '[ Location 1 ] (V 3 Float)
   , "out_position" ':-> Output     '[ Location 0 ] (V 4 Float)
   , "out_normal"   ':-> Output     '[ Location 1 ] (V 4 Float)

    -- Descriptor Set 0 : Pipeline properties (bound once per pipeline)
   , "camera"       ':-> Uniform    '[ DescriptorSet 0, Binding 0 ]
                          (Struct [ "view_matrix" ':-> M 4 4 Float
                                  , "proj_matrix" ':-> M 4 4 Float
                                  ])
    -- Descriptor Set 1 : Material properties (bound once per material)
    -- ...

    -- Descriptor Set 2 : Mesh properties (bound once per mesh)
   , "model"        ':-> Uniform    '[ DescriptorSet 2, Binding 0 ] (Struct '[ "m" ':-> M 4 4 Float ])
   ]


vertex :: G.VertexShaderModule VertexDefs _
vertex = shader do
    ~(Vec3 x y z)    <- #in_position
    ~(Vec3 nx ny nz) <- #in_normal
    proj_matrix <- use @(Name "camera" :.: Name "proj_matrix")
    view_matrix <- use @(Name "camera" :.: Name "view_matrix")
    model_mat   <- use @(Name "model" :.: Name "m")
    let view_p = (view_matrix !*! model_mat) !*^ (Vec4 x y z 1)
    #gl_Position  .= proj_matrix !*^ view_p
    #out_position .= view_p
    #out_normal   .= (view_matrix !*! model_mat) !*^ (Vec4 nx ny nz 0) -- careful! don't translate (hence the w=0)

fragment :: G.FragmentShaderModule
  '[ "in_position"  ':-> Input      '[ Location 0 ] (V 4 Float) -- in view space
   , "in_normal"    ':-> Input      '[ Location 1 ] (V 4 Float) -- in view space
   ] _
fragment = shader do
  let lightDir  = Vec3 0 (-1) (-1)
  let lightCol  = Vec3 0.3 0.3 0.3
  let objectCol = Vec3 1 1 1
  let shininess = 64

  lightValue <- blinnPhong 0.01 shininess 1 lightDir lightCol

  let Vec3 colx coly colz
        = gammaCorrection defaultGamma (lightValue `pointwiseMult` objectCol)

  #out_colour .= Vec4 colx coly colz 1

  -- ~(Vec4 nx ny nz _) <- #in_normal
  -- #out_colour .= Vec4 (nx*0.5 + 0.5) (ny*0.5 + 0.5) (nz*0.5 + 0.5) 1

