{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE BlockArguments   #-} -- cleaner syntax for 'do' blocks (optional)
{-# LANGUAGE DataKinds        #-} -- for datatype promotion and type-level literals
{-# LANGUAGE RebindableSyntax #-} -- 'do' notation to create ASTs with indexed monads
{-# LANGUAGE TypeApplications #-} -- to specify type-level arguments
{-# LANGUAGE TypeOperators    #-} -- for type operators such as ':->, which stands for key/value assignment
module Ghengin.Shader.Lighting where

import FIR
import Math.Linear

blinnPhong :: ∀ π
            . ( V 4 Float ~ Has "in_position" π
              , V 4 Float ~ Has "in_normal" π

              , CanGet "in_position" π
              , CanGet "in_normal" π
              , CanGet "ubo" π

              , _ -- extra constraints (wildcard at the end)
              )
           => Code Float -> Code (V 3 Float) -> Program π π (Code (V 3 Float))
blinnPhong specularity col = do

    ~(Vec4 px py pz _) <- get @"in_position" @(V 4 Float) @π
    ~(Vec4 nx ny nz _) <- get @"in_normal"   @(V 4 Float) @π
    ~(Vec3 cx cy cz)   <- use @(Name "ubo" :.: Name "camera_pos")

    let

        -- Light
        viewDir    = normalise (Vec3 cx cy cz ^-^ Vec3 px py pz)
        dirToLight = normalise (Vec3 1 (-3) (-1))
        ambient    = 0.05 *^ col
        normal     = normalise (Vec3 nx ny nz)
        -- light intensity given by cosine of direction to light and the normal in world space
        diffuse    = (max (dot dirToLight normal) 0) *^ col
        halfwayDir = normalise (dirToLight ^+^ viewDir)
        specular   = ((max (dot halfwayDir normal) 0) ** specularity) *^ (Vec3 0.3 0.3 0.3 {- bright light -})

        Vec3 colx coly colz = ambient ^+^ diffuse ^+^ specular

     in
        pure $ Vec3 colx coly colz

