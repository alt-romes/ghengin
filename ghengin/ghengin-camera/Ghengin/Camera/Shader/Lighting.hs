{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE BlockArguments   #-} -- cleaner syntax for 'do' blocks (optional)
{-# LANGUAGE DataKinds        #-} -- for datatype promotion and type-level literals
{-# LANGUAGE RebindableSyntax #-} -- 'do' notation to create ASTs with indexed monads
{-# LANGUAGE TypeApplications #-} -- to specify type-level arguments
{-# LANGUAGE TypeOperators    #-} -- for type operators such as ':->, which stands for key/value assignment
module Ghengin.Camera.Shader.Lighting where

import FIR
import Math.Linear

-- | Receives @in_position@ and @in_normal@ in view space coordinates (so that the viewer is at (0,0,0))
blinnPhong :: ∀ π
            . ( V 4 Float ~ Has "in_position" π
              , V 4 Float ~ Has "in_normal" π

              , CanGet "in_position" π
              , CanGet "in_normal" π

              , _ -- extra constraints (wildcard at the end)
              )
           => Code Float       -- ^ Ambient strength
           -> Code Float       -- ^ Shininess
           -> Code (V 3 Float) -- ^ Direction to light. E.g. computed as @lightPos - in_position@
           -> Code (V 3 Float) -- ^ Light color
           -> Program π π (Code (V 3 Float))
           -- ^ Returns the light color value. Don't forget to multiply by object color!
blinnPhong ambientStrength shininess unNormalLightDir lightColor = do

    ~(Vec4 px py pz _) <- get @"in_position" @(V 4 Float) @π
    ~(Vec4 nx ny nz _) <- get @"in_normal"   @(V 4 Float) @π

    let
        ambient    = ambientStrength

        normalVec  = normalise (Vec3 nx ny nz)
        lightDir   = normalise unNormalLightDir
        viewDir    = normalise (Vec3 (-px) (-py) (-pz)) -- (bc. viewPos is (0,0,0))
        halfwayDir = normalise (lightDir ^+^ viewDir)
        -- light intensity given by cosine of direction to light and the normal in world space
        diffuse    = (max (dot normalVec lightDir) 0)
        specular   = ((max (dot normalVec halfwayDir) 0) ** shininess)

        Vec3 colx coly colz = (ambient + diffuse + (specular{- * spec strength? -})) *^ lightColor

     in
        pure $ Vec3 colx coly colz

--------------------------------------------------------------------------------
-- Basic Lighting
-- See https://learnopengl.com/Lighting/Basic-Lighting
--------------------------------------------------------------------------------

-- | Compute ambient lighting from given params
ambientLight :: Code Float       -- ^ Ambient light strength
             -> Code (V 3 Float) -- ^ Light color
             -> Code (V 3 Float) -- ^ Resulting color
ambientLight ambientStrength lightColor
  = ambientStrength *^ lightColor

-- | Compute diffuse lighting from given params
--
-- Notes: @in_position@ and @in_normal@ should be in view space.
diffuseLight :: ∀ π
            . ( V 4 Float ~ Has "in_position" π
              , V 4 Float ~ Has "in_normal" π

              , CanGet "in_position" π
              , CanGet "in_normal" π

              , _ -- extra constraints (wildcard at the end)
              )
           => Code (V 3 Float) -- ^ Light position in view space (view <> model)
           -> Code (V 3 Float) -- ^ Light color
           -> Program π π (Code (V 3 Float))
diffuseLight lightPos lightColor = do

    ~(Vec4 px py pz _) <- get @"in_position" @(V 4 Float) @π
    ~(Vec4 nx ny nz _) <- get @"in_normal"   @(V 4 Float) @π

    let
      normVec  = normalise (Vec3 nx ny nz)
      lightDir = normalise (lightPos ^-^ Vec3 px py pz)
      diff     = max (normVec ^.^ lightDir) 0 :: Code Float
      diffuse  = diff *^ lightColor :: Code (V 3 Float)
     in
      pure diffuse

-- | Compute Phong specular highlights from the given parameters.
-- Positions are given in View Space.
--
-- NB: Because it is in view space, the camera/viewer position is always (0,0,0).
--
-- The OpenGL tutorial notes "... but most people tend to prefer doing lighting
-- in view space. An advantage of view space is that the viewer's position is
-- always at (0,0,0) so you already got the position of the viewer for free."
specularLight :: ∀ π
            . ( V 4 Float ~ Has "in_position" π
              , V 4 Float ~ Has "in_normal" π

              , CanGet "in_position" π
              , CanGet "in_normal" π

              , _ -- extra constraints (wildcard at the end)
              )
              => Code Float -- ^ Specular strength
              -> Code Float -- ^ "Shininess"
              -> Code (V 3 Float) -- ^ Light position in view space
              -> Code (V 3 Float) -- ^ Light color
              -> Program π π (Code (V 3 Float))
specularLight specularStrength shininess lightPos lightCol = do

    ~(Vec4 px py pz _) <- get @"in_position" @(V 4 Float) @π
    ~(Vec4 nx ny nz _) <- get @"in_normal"   @(V 4 Float) @π

    let
      fragPos    = Vec3 px py pz
      normVec    = normalise (Vec3 nx ny nz)
      lightDir   = normalise (lightPos ^-^ fragPos)
      viewDir    = normalise ({-viewPos - fragPos <=>-}(-1) *^ fragPos)
      reflectDir = reflect' (lightDir) normVec
      spec       = (max (dot viewDir reflectDir) 0) ** shininess
     in
      pure $ (specularStrength * spec) *^ lightCol

--- Utils ----------------------------------------------------------------------

pointwiseMult :: Code (V 3 Float) -> Code (V 3 Float) -> Code (V 3 Float)
pointwiseMult (Vec3 ax ay az) (Vec3 bx by bz) = Vec3 (ax*bx) (ay*by) (az*bz)

