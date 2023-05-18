{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE QualifiedDo #-}
module Planet where

import Apecs.Linear (Entity(..), cmapM)
import Ghengin.Core.Log
import Data.Unrestricted.Linear (liftUrT, runUrT, UrT(..))
import qualified Control.Monad as M
import Control.Functor.Linear as Linear
import Control.Monad.IO.Class.Linear as Linear
import qualified Data.Counted as Counted
import Data.IORef (IORef)
import Data.List (foldl')
import Data.String
import Data.Typeable
import Foreign.Ptr
import Foreign.Storable.Generic
import GHC.Float
import GHC.Generics
import GHC.Records
import GHC.TypeLits
import Ghengin hiding (get)
import Ghengin.Component.Mesh.Sphere
import Ghengin.Component.UI
import Ghengin.Core.Material
import Ghengin.Core.Mesh
import Ghengin.Core.Mesh.Vertex
import Ghengin.Core.Render.Packet
import Ghengin.Core.Render.Property
import Ghengin.Core.Type.Compatible
import Ghengin.Core.Type.Utils (Sized(..))
import Ghengin.DearImGui.Gradient
import Ghengin.Shader.FIR as FIR ((:->)(..), Struct, Syntactic(..))
import Ghengin.Vulkan.Renderer
import Ghengin.Vulkan.Renderer.Kernel
import Ghengin.Vulkan.Renderer.DescriptorSet
import Ghengin.Vulkan.Renderer.Sampler
import Ghengin.Vulkan.Renderer.Texture
import Prelude ((*), (+), (-), (/))
import Prelude.Linear hiding (All, IO, get, (*), (+), (-), (/))
import System.IO.Linear
import System.Random
import Unsafe.Coerce
import qualified Unsafe.Linear as Unsafe
import qualified Apecs.Linear as C
import qualified Data.List.NonEmpty as NE
import qualified Foreign.Storable as S
import qualified Ghengin.Shader.FIR as FIR
import qualified Prelude

import Noise
import Shader (CameraProperty)
import qualified Shader

-- MOVE SOMEWHERE USEFUL
-- is the Unsafe.toLinear worse for performance than doing this right? It might block some optimizations
instance Consumable Vec3 where
  consume = Unsafe.toLinear \x -> ()
instance Dupable Vec3 where
  dup2 = Unsafe.toLinear \x -> (x,x)
instance Movable Vec3 where
  move = Unsafe.toLinear \x -> Ur x

type Planet = RenderPacket
type PlanetProps = '[MinMax,Texture2D]

data MinMax = MinMax !Float !Float
  deriving (Prelude.Eq, Generic, Show)

instance Syntactic MinMax where
  type Internal MinMax = FIR.Val (Struct '[ "min" ':-> Float, "max" ':-> Float ])
  toAST (MinMax x y) = FIR.Struct (FIR.Lit x FIR.:& FIR.Lit y FIR.:& FIR.End)
  fromAST struct = case (FIR.view @(FIR.Name "min") struct, FIR.view @(FIR.Name "max") struct) of
                     (FIR.Lit x, FIR.Lit y) -> MinMax x y
                     _ -> error "impossible"

instance GStorable MinMax
instance Sized MinMax where
  type SizeOf MinMax = 2 * SizeOf Float

planetPBS :: MinMax -> RefC Texture2D ⊸ PropertyBindings PlanetProps
planetPBS mm t = StaticBinding (Ur mm) :## Texture2DBinding t :## GHNil

data PlanetSettings = PlanetSettings { resolution :: !(IORef Int)
                                     , radius     :: !(IORef Float)
                                     , color      :: !(IORef Vec3)
                                     , useFirstLayerAsMask :: !(IORef Bool)
                                     , noiseSettings :: !(NE.NonEmpty NoiseSettings)
                                     , displayFace   :: !(IOSelectRef DisplayFace)
                                     , gradient :: ImGradient
                                     }

data DisplayFace = All | FaceUp | FaceRight deriving Show

instance UISettings PlanetSettings where

  type ReactivityInput PlanetSettings = Entity
  type ReactivityOutput PlanetSettings = ()
  type ReactivityConstraints PlanetSettings w = Dupable w

  makeSettings = Linear.do
    Ur resR   <- newIORef 5
    Ur radR   <- newIORef 1
    Ur colorR <- newIORef (vec3 1 0 0)
    Ur boolR  <- newIORef False
    Ur ns1    <- makeSettings @NoiseSettings
    Ur ns2    <- makeSettings @NoiseSettings
    Ur _ns3    <- makeSettings @NoiseSettings
    Ur df     <- newIOSelectRef All
    Ur grad <- liftSystemIOU $ newGradient (vec3 0 0 0) (vec3 1 1 1)
    pure $ Ur $ PlanetSettings resR radR colorR boolR [ns1, ns2] df grad

  makeComponents ps@(PlanetSettings re ra co bo nss df grad) planetEntity = Linear.do

    Ur (RenderPacket (oldMesh :: Mesh ms) (Ref mat_ref :: Ref (Material mt)) pp _) <- C.get planetEntity
    Ur (SomeMaterial (unsafeCoerce -> mat :: Material mt)) <- C.get (Entity mat_ref)
      -- BIG:TODO: Move unsafe coerce to definition of "get_material" that may
      -- from a statically known reference extract a statically known material
      -- safely

    -- Local equality for this render packets being a PlanetMaterial by
    -- comparing the runtime tag of this render packet.
    --
    -- > Therefore, GADT matches simply act as a rigid “type information diode”:
    --   the stuff inside can never be used to determine types on the outside,
    --   like that the case block as a whole should be IO ().
    --
    -- TODO: Some nice workaround to avoid this?
    () <- case (eqT @mt @PlanetProps, eqT @ms @[Vec3, Vec3, Vec3]) of
      (Just Refl, Just Refl) -> Linear.do

        withTree "Planet" Linear.do
          Ur _b1 <- sliderInt "Resolution" re 2 200
          Ur _b2 <- sliderFloat "Radius" ra 0 3

          Ur gb <- liftSystemIOU $ gradientButton grad
          Ur ge <- liftSystemIOU $ gradientEditor grad
          if ge then Linear.do

            -- Because we introduced the equality constraint we can now edit
            -- the material AND we still keep the 'Compatible' instance because
            -- we simply introduced an equality to edit the material, and
            -- didn't remove the original compatibility between the existential
            -- material and pipeline
            newTex <- textureFromGradient grad
            newMat <- lift $ mat & propertyAt @1 (\oldtex -> Counted.forget oldtex >> pure newTex) -- linearity makes us forget the reference counted previous texture
            Unsafe.toLinear2 C.set (Entity mat_ref) (SomeMaterial newMat) -- TODO: wrap calls to C.set -- ROMES:TODO: Unsafe set, just to maek this work. The whole linear apecs thing is kind of broken.
-- TODO: For now we have to do this manually since re-using the same mesh but
-- building a new render packet will make the reference count of the same mesh
-- be incorrectly increased. Perhaps we could have a similar function which takes some parameters.
            -- ^^^ decRefCount oldMesh 
            -- We no longer need to do this. Now, we only need to recreate the
            -- render packet if we want to change the material this render
            -- packet is using.
            -- However, if we want to change the material (and have the effect
            -- reflected on all packets with this material), we simply change
            -- it.
            -- C.set planetEntity =<< renderPacket oldMesh newMat pp

            -- The textures are now reference counted and the discarded one is
            -- freed and the new one's reference count is increased when
            -- updated through medit

            pure ()
          else pure ()

          Ur _b4 <- checkBox "Mask" bo
          Ur _b5 <- withCombo "Faces" df [All, FaceUp, FaceRight]
          pure ()

        consume <$> runUrT (M.mapM (\(ns, i) -> liftUrT $
          withTree ("Layer " Prelude.<> fromString (show i)) $ makeComponents ns ()) (NE.zip nss [1..]))

        whenM (unur <$> button "Generate") $ Linear.do

           (newMesh,newMinMax) <- newPlanetMesh ps

            -- We can enforce this by ensuring oldMesh is brought linearly into scope
           lift $ freeMesh oldMesh -- Can we hide/enforce this somehow?
                                   -- Meshes aren't automatically freed when switched! We should make "switching" explicit?
                                   -- ^ Perhaps pattern matching on a mesh pattern synonym of the mesh would free it

           -- Edit multiple material properties at the same time
           -- newMaterial <- lift $ medits @[0,1] @PlanetProps mat $ (\_oldMinMax -> newMinMax) :-# pure :+# HFNil
           newMaterial <- lift $ mat & propertyAt @0 (\(Ur oldMinMax) -> pure newMinMax)
           Unsafe.toLinear2 C.set (Entity mat_ref) (SomeMaterial newMaterial) -- ROMES:TODO: Linearity broken on surface+apecs... rewrite whole frontend

           -- Here we have to recreate the packet because a mesh is currently not a ref
           renderPacket @_ @_ @mt @_ newMesh (Ref mat_ref) pp >>= Unsafe.toLinear2 C.set planetEntity -- ignore linearity wrt apecs, it's all broken needs fix
      _ -> error "Not a planet material nor mesh BOOM"

    pure ()

textureFromGradient :: Dupable w => ImGradient -> Ghengin w (RefC Texture2D)
textureFromGradient grad = Linear.do
  let img = generateImage (\x _y -> normVec3ToRGB8 $ colorAt grad (fromIntegral x/(50-1))) 50 1
  sampler <- lift $ createSampler FILTER_NEAREST SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
  lift $ textureFromImage (ImageRGB8 img) sampler


newPlanet :: ∀ p w. (Dupable w, Typeable p, Compatible '[Vec3,Vec3,Vec3] PlanetProps '[CameraProperty] p)
          => PlanetSettings -> RenderPipeline p '[CameraProperty] ⊸ Ref (RenderPipeline p '[CameraProperty]) -> Ghengin w Planet
newPlanet ps@(PlanetSettings re ra co bo nss df grad) pipeline pipelineRef = Linear.do
  logT "Making mesh"
  (mesh,Ur minmax) <- newPlanetMesh ps
  logT "Making gradient"
  tex <- textureFromGradient grad
  logT "Making material"
  (mat, pipeline') <- lift (material @PlanetProps @p (planetPBS minmax tex) pipeline)
  Ur (Entity matref) <- Unsafe.toLinear C.newEntity (SomeMaterial mat) -- ROMES:TODO: Apecs linearity is broken.
  Unsafe.toLinear (\_ -> pure ()) pipeline' -- rOMES:TODO: surface level linearity is broken
  logT "Making render packet"
  rp <- renderPacket @p @_ @PlanetProps mesh (Ref matref) pipelineRef
  logT "Done"
  pure rp

newPlanetMesh :: Dupable w => PlanetSettings -> Ghengin w (Mesh '[Vec3, Vec3, Vec3], Ur MinMax)
newPlanetMesh (PlanetSettings re ra co bo nss df grad) = lift $ Linear.do
  Ur re' <- liftIO $ readIORef re
  Ur ra' <- liftIO $ readIORef ra
  Ur co' <- liftIO $ readIORef co
  Ur df' <- liftIO $ readIOSelectRef df
  Ur enableMask <- liftIO $ readIORef bo

  let (vs, is) = case df' of
                   All -> let UnitSphere v i = newUnitSphere re' (Just co') in (v, i)
                   FaceUp -> let UF v i = newUnitFace re' (vec3 0 (-1) 0)
                              in (Prelude.zipWith3 (\a b c -> a :& b :&: c) v (calculateSmoothNormals i v) (Prelude.repeat co'),i)
                   FaceRight -> let UF v i = newUnitFace re' (vec3 1 0 0)
                              in (Prelude.zipWith3 (\a b c -> a :& b :&: c) v (calculateSmoothNormals i v) (repeat co'),i)

  Ur (ps', elevations) <- runUrT $ Prelude.unzip Prelude.<$> M.forM vs \(p :& _) -> liftUrT $
    case nss of
      ns NE.:| nss' -> Linear.do
        Ur initialElevation <- liftIO $ runUrT $ evalNoise ns p
        let mask = if enableMask then initialElevation else 1
        Ur noiseElevation <- liftIO $ runUrT $ M.foldM (\acc ns' -> (\x -> acc+x*mask) Prelude.<$> evalNoise ns' p) initialElevation nss'
        let elevation = ra' * (1 + noiseElevation)
        pure (p ^* elevation, elevation)

  let
      ns' = calculateSmoothNormals is ps'
      cs  = Prelude.map (\(_ :& _ :&: c) -> c) vs
      vs'' = Prelude.zipWith3 (\a b c -> a :& b :&: c) ps' ns' cs

      minmax = MinMax (Prelude.minimum elevations) (Prelude.maximum elevations)
   in (,Ur minmax) <$> createMeshWithIxs vs'' is

-- Utilities

whenM :: Monad m => m Bool -> m () -> m ()
whenM c t = Linear.do
  b <- c
  if b then t
       else pure ()

