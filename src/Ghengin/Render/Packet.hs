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

-- data RenderPacket = forall info.
--                     RenderPacket { _renderPipeline :: RenderPipeline info
--                                  , _renderMesh     :: Mesh
--                                  , _renderMaterial :: Material
--                                  }

-- instance Component RenderPacket where
--   type Storage RenderPacket = Map RenderPacket

-- instance (Monad m, HasField "renderPackets" w (Storage RenderPacket)) => Has w m RenderPacket where
--   getStore = SystemT (asks (.renderPackets))



