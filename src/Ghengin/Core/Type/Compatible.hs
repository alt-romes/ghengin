module Ghengin.Core.Type.Compatible where

import Data.Kind
import FIR.Pipeline

data PropertyKind = VertexProperty | MaterialProperty | RenderProperty

type Compatible :: PropertyKind -- ^ The kind of property (vertex, material, or render property)
                -> [Type]       -- ^ The properties
                -> PipelineInfo -- ^ The pipeline against which the properties must be compatible
                -> Constraint
type family Compatible κ αs π where


