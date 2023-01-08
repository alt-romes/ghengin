module Ghengin.Shaders.Fixed where

-- Descriptor Set #0 for things bound once per pipeline (global pipeline data)
-- Descriptor Set #1 for things bound once per material
-- Descriptor Set #2 for things bound once per object

---- Vertex -----

-- type VertexDefs
--   = '[ "in_position" ':-> Input '[ Location 0 ] (V 3 Float)
--      , "in_normal"   ':-> Input '[ Location 1 ] (V 3 Float)
--      , "out_position" ':-> Output '[ Location 0 ] (V 4 Float)
--      , "out_normal"   ':-> Output '[ Location 1 ] (V 4 Float)
--      , "push"        ':-> PushConstant '[] (Struct '[ "model" ':-> M 4 4 Float ])
--      , "ubo"         ':-> Uniform '[ DescriptorSet 0, Binding 0 ]
--                                   ( Struct '[ "view" ':-> M 4 4 Float
--                                             , "proj" ':-> M 4 4 Float ] )
--      , "main"        ':-> EntryPoint '[] Vertex
--      ]
