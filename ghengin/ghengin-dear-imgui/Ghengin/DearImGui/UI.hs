{-# LANGUAGE OverloadedStrings, DefaultSignatures #-}
module Ghengin.DearImGui.UI
  ( module Ghengin.DearImGui.UI
  , module Data.Default
  ) where

import GHC.TypeNats
import Data.Kind
import GHC.Real
import Data.Default
import qualified Data.Text as T
import Control.Monad
import Data.IORef
import Geomancy.Vec3
import Prelude
import Data.Bifunctor
import Generics.SOP

import qualified DearImGui as ImGui

class Widget a where
  widget :: a -> IO (a, Bool)

  default widget :: (Generic a, HasDatatypeInfo a, All2 Widget (Code a), All2 Default (Code a))
                 => a -> IO (a, Bool)
  widget = genericUI

-- | Generic UI generator using metadata
genericUI :: forall a. (Generic a, HasDatatypeInfo a, All2 Widget (Code a))
          => All2 Default (Code a)
          => a -> IO (a, Bool)
genericUI x = do
  let info = datatypeInfo (Proxy @a)
  bimap to id <$> gUIS info (from x)

-- | Generic UI for SOP representation with metadata
gUIS :: All2 Widget xss
     => All2 Default xss
     => DatatypeInfo xss
     -> SOP I xss
     -> IO (SOP I xss, Bool)
gUIS (Newtype _ _ _) (SOP (S nope)) = case nope of {}
gUIS (Newtype _mod _dt constr) (SOP (Z prod)) =
  bimap (SOP . Z) id <$> gUIProduct False constr prod
gUIS (ADT _mod _dt (constr :* Nil) _strictness) (SOP (Z prod)) =
  bimap (SOP . Z) id <$> gUIProduct False constr prod
gUIS (ADT _mod _dt constrs _strictness) (SOP sop) =
  bimap SOP id <$> gUISum constrs sop

-- | UI for sum types (multiple constructors) with constructor selection
gUISum :: forall xss. All2 Widget xss
       => All2 Default xss
       => NP ConstructorInfo xss
       -> NS (NP I) xss
       -> IO (NS (NP I) xss, Bool)
gUISum constrs ns = do
  -- Get the current constructor index
  let currentIdx = hindex ns
      constrNames = hcollapse (hmap (K . T.pack . constructorName) constrs)
  
  -- Create combo box for constructor selection
  idxRef <- newIORef currentIdx
  constrChanged <- ImGui.combo "" idxRef constrNames
  newIdx <- readIORef idxRef
  
  -- If constructor changed, create new default value for that constructor
  ns' <- if constrChanged && newIdx /= currentIdx
         then return (createDefaultNS newIdx constrs)
         else return ns
  
  -- Create UI for the selected constructor's fields
  (ns'', fieldsChanged) <- gUINS constrs ns'

  return (ns'', (constrChanged && newIdx /= currentIdx) || fieldsChanged)
  where
    gUINS = go where
      go :: All2 Widget xss'
         => NP ConstructorInfo xss'
         -> NS (NP I) xss'
         -> IO (NS (NP I) xss', Bool)
      go (constr :* _) (Z prod) = do
        (prod', changed) <- gUIProduct False{- don't display con because it's already in constructor selector combo -}
                              constr prod
        return (Z prod', changed)
      go (_ :* constrs') (S ns') = do
        (ns'', changed) <- go constrs' ns'
        return (S ns'', changed)
      go Nil ns'' = case ns'' of {}

    -- Create a default NS at a given index (for constructor switching)
    createDefaultNS = go
      where
        go :: All2 Default xss' => All2 Widget xss'
           => Int -> NP ConstructorInfo xss' -> NS (NP I) xss'
        go 0 (_ :* _) = Z (hcpure (Proxy @Default) (I def))
        go n (_ :* cs) = S (go (n - 1) cs)
        go _ Nil = error "Invalid constructor index"

-- | UI for product types (record fields) using metadata
gUIProduct :: All Widget xs
           => Bool -- ^ Whether to display the constructor name
           -> ConstructorInfo xs
           -> NP I xs
           -> IO (NP I xs, Bool)
gUIProduct displayCon constr prod = case constr of
  -- Display constructor name as header
  Constructor name -> do
    when displayCon $
      ImGui.text $ T.pack name
    ImGui.withIndent 5 $
      gUINP 0 prod
  Infix name _assoc _fixity -> do
    when displayCon $
      ImGui.text $ T.pack name
    ImGui.withIndent 5 $
      gUINP 0 prod
  Record name fields -> do
    when displayCon $
      ImGui.text $ T.pack name
    ImGui.withIndent 5 $
      gUIRecord 0 fields prod

-- | UI for n-ary product with field names (records)
gUIRecord :: All Widget xs
          => Int  -- Current field index
          -> NP FieldInfo xs
          -> NP I xs
          -> IO (NP I xs, Bool)
gUIRecord _ Nil Nil = return (Nil, False)
gUIRecord idx (FieldInfo fieldN :* fields) (I val :* vals) = 
  ImGui.withID idx $ do
    -- Use field name as label
    ImGui.text $ T.pack fieldN
    (val', changed1) <- widget val
    
    (vals', changed2) <- gUIRecord (idx + 1) fields vals
    return (I val' :* vals', changed1 || changed2)

-- | UI for n-ary product without field names (indexed)
gUINP :: All Widget xs
      => Int  -- Current index
      -> NP I xs
      -> IO (NP I xs, Bool)
gUINP _ Nil = return (Nil, False)
gUINP idx (I val :* vals) = 
  ImGui.withID idx $ do
    ImGui.text $ T.pack ("[" ++ show idx ++ "]")
    ImGui.sameLine
    (val', changed1) <- widget val
    
    (vals', changed2) <- gUINP (idx + 1) vals
    return (I val' :* vals', changed1 || changed2)

-- Base type instances
instance Widget Int where
  widget val = do
    ref <- newIORef val
    changed <- ImGui.sliderInt "##int" ref minBound maxBound
    newVal <- readIORef ref
    return (newVal, changed)

instance Widget Float where
  widget val = do
    ref <- newIORef val
    changed <- ImGui.sliderFloat "##float" ref (fromIntegral (minBound @Int)) (fromIntegral (maxBound @Int))
    newVal <- readIORef ref
    return (newVal, changed)

instance Widget Double where
  widget val = do
    ref <- newIORef (realToFrac val :: Float)
    changed <- ImGui.sliderFloat "##double" ref (fromIntegral (minBound @Int)) (fromIntegral (maxBound @Int))
    newVal <- readIORef ref
    return (realToFrac newVal, changed)

-- Vec3 instance
instance Widget Vec3 where
  widget (WithVec3 x y z) = do
    ref <- newIORef (x,y,z)
    c1 <- ImGui.sliderFloat3 "" ref (fromIntegral (minBound @Int)) (fromIntegral (maxBound @Int))
    (newX,newY,newZ) <- readIORef ref
    return (vec3 newX newY newZ, c1)

instance (Default a, Widget a) => Widget [a]

--------------------------------------------------------------------------------
-- * Default values for widgets
--------------------------------------------------------------------------------

-- orphan
instance Default Vec3 where
  def = vec3 def def def

--------------------------------------------------------------------------------
-- * Newtypes for constraining UIs
--------------------------------------------------------------------------------

-- | Display Vec3 as a color edit
newtype Color = Color { colorVec :: Vec3 }
  deriving (Eq, Ord, Show, Default)

instance Widget Color where
  widget (Color (WithVec3 x y z)) = do
    ref <- newIORef (ImGui.ImVec3 x y z)
    
    c1 <- ImGui.colorEdit3 "" ref
    
    ImGui.ImVec3 newX newY newZ <- readIORef ref
    return (Color $ vec3 newX newY newZ, c1)

-- | A value between low and high
newtype InRange (low :: Nat) (high :: Nat) (a :: Type) = InRange { inRangeVal :: a }
  deriving (Eq, Ord, Show, Read, Num, Fractional, Real, Default)

-- Base type instances
instance (KnownNat low, KnownNat high) => Widget (InRange low high Int) where
  widget (InRange val) = do
    ref <- newIORef val
    changed <- ImGui.sliderInt "##int" ref (fromIntegral $ natVal (Proxy @low)) 
                                           (fromIntegral $ natVal (Proxy @high))
    newVal <- readIORef ref
    return (InRange newVal, changed)

instance (KnownNat low, KnownNat high) => Widget (InRange low high Float) where
  widget (InRange val) = do
    ref <- newIORef val
    changed <- ImGui.sliderFloat "##float" ref (fromIntegral $ natVal (Proxy @low)) 
                                               (fromIntegral $ natVal (Proxy @high))
    newVal <- readIORef ref
    return (InRange newVal, changed)

instance (KnownNat low, KnownNat high) => Widget (InRange low high Double) where
  widget (InRange val) = do
    ref <- newIORef (realToFrac val)
    changed <- ImGui.sliderFloat "##double" ref (fromIntegral $ natVal (Proxy @low)) 
                                                (fromIntegral $ natVal (Proxy @high))
    newVal <- readIORef ref
    return (InRange (realToFrac newVal), changed)

-- TODO:
--  - sliderAngle
