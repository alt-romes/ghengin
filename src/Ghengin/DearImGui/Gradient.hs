{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Ghengin.DearImGui.Gradient where

import Geomancy.Vec3
import System.IO.Unsafe
-- import Data.StateVar
import DearImGui.Raw.Context
  ( imguiContext )

import Foreign.Storable
import Foreign.Marshal.Array
-- import Foreign.Marshal.Utils
import Control.Monad.IO.Class
import Foreign.C.Types
import Foreign.Ptr
import qualified Language.C.Inline.Cpp as Cpp
-- import DearImGui

-- TODO: Possibly a native gradient format that would create and pass an ImGradient instance to the functions? Probably not better...
-- TODO: Delete instances...
data ImGradient = ImGradient (Ptr ImGradient)
data ImGradientMark = ImGradientMark { r,g,b,a,pos :: {-# UNPACK #-} !Float }

instance Storable ImGradientMark where
  sizeOf _ = 5 * sizeOf @Float undefined
  alignment _ = 0

  poke ptr ImGradientMark{ r, g, b, a, pos } = do
    poke (castPtr ptr `plusPtr` (sizeOf r * 0)) r
    poke (castPtr ptr `plusPtr` (sizeOf r * 1)) g
    poke (castPtr ptr `plusPtr` (sizeOf r * 2)) b
    poke (castPtr ptr `plusPtr` (sizeOf r * 3)) a
    poke (castPtr ptr `plusPtr` (sizeOf r * 4)) pos

  peek ptr = do
    r   <- peek (castPtr ptr                         )
    g   <- peek (castPtr ptr `plusPtr` (sizeOf r * 1))
    b   <- peek (castPtr ptr `plusPtr` (sizeOf r * 2))
    a   <- peek (castPtr ptr `plusPtr` (sizeOf r * 3))
    pos <- peek (castPtr ptr `plusPtr` (sizeOf r * 4))
    pure ImGradientMark{ r, g, b, a, pos }

Cpp.context (Cpp.cppCtx <> Cpp.bsCtx <> imguiContext <>
  Cpp.cppTypePairs [
    ("ImGradient", [t|ImGradient|]),
    ("ImGradientMark", [t|ImGradientMark|])
    ]
  )
Cpp.include "imgui_color_gradient.hpp"
Cpp.using "namespace ImGui"

-- TODO: When will it be freed?
newGradient :: (MonadIO m) => Vec3 -> Vec3 -> m ImGradient
newGradient (WithVec3 (CFloat -> x1) (CFloat -> y1) (CFloat -> z1))
            (WithVec3 (CFloat -> x2) (CFloat -> y2) (CFloat -> z2)) = liftIO do
  pt <- [Cpp.block| ImGradient* {
                      ImGradient* gradient = new ImGradient();
                      gradient->addMark(0.0f, ImColor($(float x1), $(float y1), $(float z1)));
                      gradient->addMark(1.0f, ImColor($(float x2), $(float y2), $(float z2)));
                      return gradient; } |]
  pure (ImGradient pt)

woodGradient :: (MonadIO m) => m ImGradient
woodGradient = liftIO do
  pt <- [Cpp.block| ImGradient* { ImGradient* gradient = new ImGradient();
                                   gradient->getMarks().clear();
                                   gradient->addMark(0.0f, ImColor(0xA0, 0x79, 0x3D));
                                   gradient->addMark(0.2f, ImColor(0xAA, 0x83, 0x47));
                                   gradient->addMark(0.3f, ImColor(0xB4, 0x8D, 0x51));
                                   gradient->addMark(0.4f, ImColor(0xBE, 0x97, 0x5B));
                                   gradient->addMark(0.6f, ImColor(0xC8, 0xA1, 0x65));
                                   gradient->addMark(0.7f, ImColor(0xD2, 0xAB, 0x6F));
                                   gradient->addMark(0.8f, ImColor(0xDC, 0xB5, 0x79));
                                   gradient->addMark(1.0f, ImColor(0xE6, 0xBF, 0x83));
                                  return gradient;
                                } |]
  pure (ImGradient pt)


colorAt :: ImGradient
        -> Float -- ^ Position from 0 to 1
        -> Vec3  -- ^ Normalised vec3
colorAt (ImGradient ptr) (CFloat -> pos) = unsafePerformIO do
  withArray [0,0,0] \(colorPtr :: Ptr CFloat) -> do

    [Cpp.exp| void { $(ImGradient* ptr)->getColorAt($(float pos), $(float colorPtr[3])); } |]

    [x, y, z] <- peekArray 3 colorPtr

    return (vec3 (realToFrac x) (realToFrac y) (realToFrac z))

-- | ImGui gradient button: Must be defined in a UI window (or otherwise the imgui context won't be set)
gradientButton :: MonadIO m => ImGradient -> m Bool
gradientButton (ImGradient ptr) = liftIO do
  (0 /=) <$> [Cpp.exp| bool { GradientButton($(ImGradient* ptr)) } |]


-- | ImGui gradient editor: Must be defined in a UI window (or otherwise the imgui context won't be set)
gradientEditor :: (MonadIO m) -- , HasGetter ref ImGradientMark, HasSetter ref ImGradientMark)
               => ImGradient
               -- -> ref
               -- -> ref
               -> m Bool
gradientEditor (ImGradient grad) = liftIO do
  -- m1 <- get ref1
  -- m2 <- get ref2
  -- TODO: Don't use static, we should pass some ptr that can be changed to
  -- another pointer but this should still all work. Meaning we'd need to
  -- store instead the ptr to the mark inside the imgradient?
  -- For now, OK..
  -- with m1 $ \m1ptr ->
  --   with m2 $ \m2ptr -> do
      changed <- (0 /=) <$>
        [Cpp.block|
          bool {
          static ImGradientMark* draggingMark = nullptr;
          static ImGradientMark* selectedMark = nullptr;
          return
            GradientEditor( $(ImGradient* grad)
                          , draggingMark
                          , selectedMark
                          );
          }
          |]

      -- when changed $ do
      --   m1new <- peek m1ptr
      --   m2new <- peek m2ptr
      --   ref1 $=! m1new
      --   ref2 $=! m2new

      pure changed


