module Ghengin.Shader.Utils where

import qualified FIR

invLerp :: FIR.DivisionRing a => a -> a -> a -> a
invLerp value from to = (value FIR.- from) FIR./ (to FIR.- from)

lerp :: FIR.Ring a => a -> a -> a -> a
lerp from to value = from FIR.+ (to FIR.- from) FIR.* value

