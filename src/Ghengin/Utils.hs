{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeApplications #-}
module Ghengin.Utils where

import Data.Bits

(.&&.) :: Bits a => a -> a -> Bool
x .&&. y = (/= zeroBits) (x .&. y)

