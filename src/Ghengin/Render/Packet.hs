{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-} -- instance Has w m RenderPacket
module Ghengin.Render.Packet
  ( module Ghengin.Render.Packet
  , module Ghengin.Render.Pipeline
  ) where

import Ghengin.Render.Pipeline
import Ghengin.Component.Material
import Ghengin.Component.Mesh

data RenderPacket where
  -- TODO:
  --  * CompatibleMaterial mat pipeline
  --  * Mesh parametrized over type that is also validated against pipeline
  RenderPacket :: ∀ α β. Mesh -> Material α -> RenderPipeline β -> RenderPacket

-- TODO: Each render packet is then assigned with an ID and sorted in an optimal draw order.

-- data RenderPacket = forall info.
--                     RenderPacket { _renderPipeline :: RenderPipeline info
--                                  , _renderMesh     :: Mesh
--                                  , _renderMaterial :: Material
--                                  }

-- instance Component RenderPacket where
--   type Storage RenderPacket = Map RenderPacket

-- instance (Monad m, HasField "renderPackets" w (Storage RenderPacket)) => Has w m RenderPacket where
--   getStore = SystemT (asks (.renderPackets))



