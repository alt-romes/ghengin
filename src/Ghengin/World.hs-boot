{-# LANGUAGE RoleAnnotations #-}
module Ghengin.World where

import Data.Kind

data World :: Type -> Type

type role World nominal

