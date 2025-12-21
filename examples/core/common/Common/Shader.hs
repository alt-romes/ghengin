{-# OPTIONS_GHC -Wno-missing-local-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE NamedWildCards        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE OverloadedLabels      #-}
module Common.Shader where

import GHC.TypeLits
import qualified Prelude
import Math.Linear
import FIR
import FIR.AST.Prim
import FIR.Prim.Op (val, PrimTyVal)
import Data.Foldable (foldl')
import Data.List (replicate)
import Unsafe.Coerce (unsafeCoerce)
import Data.Functor (fmap)
import Data.Typeable

-- | As per Inigo's article: https://iquilezles.org/articles/fbm/
fbm2 :: Float -- ^ Num octaves
     -> Float -- ^ H parameter
     -> Code (V 2 Float)
     -> Code Float
fbm2 numOctaves _H x =
  let
    _G = exp (-_H)
    f0 = 1
    a0 = 1
    t0 = 0

    (tn, fn, an) =
      foldl' (\(t, f,a) i ->
              let
                tn = t + Lit a * noise2(Lit f *^ x)
                fn = f * 2
                an = a * _G
               in
                (tn, fn, an)
              ) (t0,f0,a0) [0..numOctaves-1]
   in tn
{-# NOINLINE fbm2 #-}


-- | The inefficient version of FBM from Inigo's article (too many pows)
fbm2_naive :: Float -- ^ Num octaves
          -> Float -- ^ H parameter
          -> Code (V 2 Float)
          -> Code Float
fbm2_naive numOctaves _H x =
  foldl' (\t i ->
    let f = 2 ** i
        a = f ** (-_H)
     in t + Lit a * noise2(Lit f *^ x)
    ) (0 :: Code Float) [0..numOctaves-1]

--------------------------------------------------------------------------------
-- * Random and Noise
--------------------------------------------------------------------------------

-- | Gradient Noise by Inigo Quilez - iq/2013
-- https://www.shadertoy.com/view/lsf3WH
--
-- Mentioned in the Book of Shaders chapter on Noise
--
-- One could also have a random texture and sample from it (might be cheaper)
noise2 :: Code (V 2 Float) -> Code Float
noise2 (Vec2 x y) = do
  let
    i = Vec2 (floor x) (floor y)
    f = Vec2 (fract x) (fract y)
    ~(Vec2 u_x u_y) = f.*f.*(Vec2 3.0 3.0 ^-^ 2.0*^f)
   in
    mix (mix (random2( i ^+^ Vec2 0.0 0.0 ))
             (random2( i ^+^ Vec2 1.0 0.0 ))
             u_x)
        (mix (random2( i ^+^ Vec2 0.0 1.0 ))
             (random2( i ^+^ Vec2 1.0 1.0 ))
             u_x)
        u_y


-- | https://iquilezles.org/articles/morenoise/
-- value_noise :: _
-- value_noise = undefined

-- How to make random for n-ary vectors?
-- random :: KnownNat n => Code (V n Float) -> Code Float
-- random v = fract(sin(dot(v, Vec2 12.989 78.233)) * 43758.543)

-- | See https://thebookofshaders.com/10/
random2 :: Code (V 2 Float) -> Code Float
random2 v =
  let
    st = Vec2 (dot v (Vec2 127.1 311.7)) (dot v (Vec2 269.5 183.3))
    -- st = v
   in
    (-1) + 2*
    fract(sin(dot st (Vec2 12.989 78.233)) * 43758.543)

--------------------------------------------------------------------------------
-- * Colors
--------------------------------------------------------------------------------

palette :: Code Float -> Code (V 3 Float) -> Code (V 3 Float) -> Code (V 3 Float) -> Code (V 3 Float) -> Code (V 3 Float)
palette t a b c d = a ^+^ (b .* cos3 (6.28318*^(c^*t ^+^ d)))

color :: Code Float -> Code (V 3 Float)
color t
  = palette t
            -- 3rd palette from https://darkeclipz.github.io/fractals/paper/Fractals%20&%20Rendering%20Techniques.html
            (Vec3 0.66 0.56 0.68)
            (Vec3 0.718 0.438 0.72)
            (Vec3 0.52 0.8 0.52)
            (Vec3 (-0.43) (-0.397) (-0.083))

color2 :: Code Float -> Code (V 3 Float)
color2 t
  = palette t
            (Vec3 0.5 0.5 0.5)
            (Vec3 0.5 0.5 0.5)
            (Vec3 1 1 1)
            (Vec3 0 0.33 0.67)

color3 :: Code Float -> Code (V 3 Float)
color3 t
  = palette t
            (Vec3 0.5 0.5 0.5)
            (Vec3 0.5 0.5 0.5)
            (Vec3 1 1 0.5)
            (Vec3 0.8 0.9 0.3)

--------------------------------------------------------------------------------
-- * Utils
--------------------------------------------------------------------------------

mix :: _ => t -> t -> t -> t
mix x y a = x*(1 - a) + y*a

fract :: (CancellativeAdditiveMonoid t, Rounding '(t,t)) => t -> t
fract x = x - floor x

-- hadarmard product, element wise vector multiplication
(.*) :: ∀ n a. (KnownNat n, ScalarTy a, AdditiveMonoid a, Floating a)
     => Code (V n a) -> Code (V n a) -> Code (V n a)
(.*) v1 v2
-- A bit contrived, but I couldn't get the generic @n@ version going.
  | Just HRefl <- heqT @n @2
  = toDiagMat2 v1 !*^ v2

  | Just HRefl <- heqT @n @3
  = toDiagMat3 v1 !*^ v2
  -- i.e. (.*) (Vec3 a b c) (Vec3 d e f) = Vec3 (a*d) (b*e) (c*f)

  | Just HRefl <- heqT @n @4
  = toDiagMat4 v1 !*^ v2

  | otherwise
  = error "unsupported size of operands for hadamard product (.*)"

toDiagMat2 :: (ScalarTy a, AdditiveMonoid a) => Code (V 2 a) -> Code (M 2 2 a)
toDiagMat2 (Vec2 a b) = Mat22 a 0
                              0 b

toDiagMat3 :: (ScalarTy a, AdditiveMonoid a) => Code (V 3 a) -> Code (M 3 3 a)
toDiagMat3 (Vec3 a b c) = Mat33 a 0 0
                                0 b 0
                                0 0 c

toDiagMat4 :: (ScalarTy a, AdditiveMonoid a) => Code (V 4 a) -> Code (M 4 4 a)
toDiagMat4 (Vec4 a b c d)
  = Mat44 a 0 0 0
          0 b 0 0
          0 0 c 0
          0 0 0 d

-- turn a vector into the diagonal of a matrix otherwise filled with 0s
-- toDiagMat :: ∀ n a. (KnownNat n, PrimTy a) => Code (V n a) -> Code (M n n a)
-- toDiagMat v = error "" -- fromAST @(V n a) v -- (fromIntegral $ val @n-1)
--   where
--     toDiagMat' :: ∀ n a
--                 . Prelude.Int -- ^ Term level representative of @n@
--                -> V n a -> M n n a
--     toDiagMat' n_term VNil = M VNil
--     toDiagMat' n_term ((:.) @_ @nminus1 x xs) =
--       let
--         -- almost...
--         zeros = fromHListVec replication (unsafeCoerce (replicate n_term 0) :: HList (Replicate nminus1 a)) :: V nminus1 a
--        in
--         M $ (x :. zeros) :. (fmap (0:.) $ unM $ (toDiagMat' @nminus1 @a (n_term-1) xs :: M nminus1 nminus1 a))


-- cos applied to all elements of a vector
cos3 :: Code (V 3 Float) -> Code (V 3 Float)
cos3 (Vec3 a b c) = Vec3 (cos a) (cos b) (cos c)

-- I think this is useless since we cannot call fract nor floor on Vec2...
instance Rounding '(a,b) => Rounding '(V n a, V n b) where
  truncate VNil = VNil
  truncate (x :. xs) = truncate x :. truncate xs

  round VNil = VNil
  round (x :. xs) = round x :. round xs

  floor VNil = VNil
  floor (x :. xs) = floor x :. floor xs

  ceiling VNil = VNil
  ceiling (x :. xs) = ceiling x :. ceiling xs

