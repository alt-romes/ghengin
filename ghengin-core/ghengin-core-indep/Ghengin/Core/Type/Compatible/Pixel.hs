module Ghengin.Core.Type.Compatible.Pixel where

import GHC.TypeError
import GHC.TypeLits
import Data.Kind ( Type, Constraint )
import SPIRV.Image
import Codec.Picture

type CompatiblePixel :: Type {-Pixel-} -> ImageFormat Nat -> Constraint
type family CompatiblePixel px fmt where
  -- RGBA
  -- RGBA8 formats
  CompatiblePixel PixelRGBA8  ('ImageFormat UNorm '[8,8,8,8]) = ()
  CompatiblePixel PixelRGBA8  ('ImageFormat SNorm '[8,8,8,8]) = ()
  CompatiblePixel PixelRGBA8  ('ImageFormat UI    '[8,8,8,8]) = ()
  CompatiblePixel PixelRGBA8  ('ImageFormat I     '[8,8,8,8]) = ()

  -- RGBA16 formats
  CompatiblePixel PixelRGBA16 ('ImageFormat UNorm '[16,16,16,16]) = ()
  CompatiblePixel PixelRGBA16 ('ImageFormat SNorm '[16,16,16,16]) = ()
  CompatiblePixel PixelRGBA16 ('ImageFormat UI    '[16,16,16,16]) = ()
  CompatiblePixel PixelRGBA16 ('ImageFormat I     '[16,16,16,16]) = ()

  -- Single channel (grayscale) formats
  -- R8 formats
  CompatiblePixel Pixel8      ('ImageFormat UNorm '[8]) = ()
  CompatiblePixel Pixel8      ('ImageFormat SNorm '[8]) = ()
  CompatiblePixel Pixel8      ('ImageFormat UI    '[8]) = ()
  CompatiblePixel Pixel8      ('ImageFormat I     '[8]) = ()

  -- R16 formats
  CompatiblePixel Pixel16     ('ImageFormat UNorm '[16]) = ()
  CompatiblePixel Pixel16     ('ImageFormat SNorm '[16]) = ()
  CompatiblePixel Pixel16     ('ImageFormat UI    '[16]) = ()
  CompatiblePixel Pixel16     ('ImageFormat I     '[16]) = ()

  -- R32 formats (no normalized)
  CompatiblePixel Pixel32     ('ImageFormat UI    '[32]) = ()
  CompatiblePixel Pixel32     ('ImageFormat I     '[32]) = ()

  -- R32F format
  CompatiblePixel PixelF      ('ImageFormat F     '[32]) = ()

  -- RG8 formats (YA8 = Y as R, A as G)
  CompatiblePixel PixelYA8    ('ImageFormat UNorm '[8,8]) = ()
  CompatiblePixel PixelYA8    ('ImageFormat SNorm '[8,8]) = ()
  CompatiblePixel PixelYA8    ('ImageFormat UI    '[8,8]) = ()
  CompatiblePixel PixelYA8    ('ImageFormat I     '[8,8]) = ()

  -- RG16 formats
  CompatiblePixel PixelYA16   ('ImageFormat UNorm '[16,16]) = ()
  CompatiblePixel PixelYA16   ('ImageFormat SNorm '[16,16]) = ()
  CompatiblePixel PixelYA16   ('ImageFormat UI    '[16,16]) = ()
  CompatiblePixel PixelYA16   ('ImageFormat I     '[16,16]) = ()

  CompatiblePixel px fmt = Unsatisfiable
                            (     Text "Incompatible "       :<>: ShowType px
                             :<>: Text " with image format " :<>: ShowType fmt
                             :<>: Text ". Take a look at 'CompatiblePixel' if you're sure they are compatible."
                             )
