{-# LANGUAGE OverloadedStrings, DefaultSignatures, DerivingStrategies, DisambiguateRecordFields, DuplicateRecordFields, DerivingVia #-}
module Ghengin.DearImGui.UI
  ( module Ghengin.DearImGui.UI
  , module Data.Default
  ) where

import GHC.TypeLits
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
  -- | UI represents this @a@ initially and returns the new value of @a@
  -- according to what the user changed and @True@ if anything did change.
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
  constrChanged <- ImGui.combo "##combo" idxRef constrNames
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
    ImGui.withIndent 5 $ do
      gUIRecord 0 fields prod

-- | UI for n-ary product with field names (records)
gUIRecord :: All Widget xs
          => Int  -- Current field index
          -> NP FieldInfo xs
          -> NP I xs
          -> IO (NP I xs, Bool)
gUIRecord _ Nil Nil = return (Nil, False)
gUIRecord !idx (FieldInfo fieldN :* fields) (I val :* vals) = do
    -- Use field name as label
    ImGui.text $ T.pack fieldN
    (val', changed1) <- ImGui.withID (T.pack fieldN) $ widget val
    
    (vals', changed2) <- gUIRecord (idx + 1) fields vals
    return (I val' :* vals', changed1 || changed2)

-- | UI for n-ary product without field names (indexed)
gUINP :: All Widget xs
      => Int  -- Current index
      -> NP I xs
      -> IO (NP I xs, Bool)
gUINP _ Nil = return (Nil, False)
gUINP !idx (I val :* vals) =
  ImGui.withID idx $ do
    -- ImGui.text $ T.pack ("[" ++ show idx ++ "]")
    -- ImGui.sameLine
    (val', changed1) <- widget val
    
    (vals', changed2) <- gUINP (idx + 1) vals
    return (I val' :* vals', changed1 || changed2)

-- Base type instances
instance Widget Int where
  widget val = do
    ref <- newIORef val
    changed <- ImGui.dragInt "##int" ref 1 (minBound @Int) (maxBound @Int)
    newVal <- readIORef ref
    return (newVal, changed)

instance Widget Float where
  widget val = do
    ref <- newIORef val
    changed <- ImGui.dragFloat "##float" ref 0.01 (fromIntegral (minBound @Int)) (fromIntegral (maxBound @Int))
    newVal <- readIORef ref
    return (newVal, changed)

instance Widget Double where
  widget val = do
    ref <- newIORef (realToFrac val :: Float)
    changed <- ImGui.dragFloat "##double" ref 0.01 (fromIntegral (minBound @Int)) (fromIntegral (maxBound @Int))
    newVal <- readIORef ref
    return (realToFrac newVal, changed)

-- vec3 instance
instance Widget Vec3 where
  widget (WithVec3 x y z) = do
    ref <- newIORef (x,y,z)
    c1 <- ImGui.dragFloat3 "##vec3" ref 0.01 (fromIntegral (minBound @Int)) (fromIntegral (maxBound @Int))
    (newX,newY,newZ) <- readIORef ref
    return (vec3 newX newY newZ, c1)

instance (Default a, Widget a) => Widget [a] where
  widget ls = do
    -- Track overall change
    changedRef <- newIORef False

    -- Header with element count and add button
    ImGui.text $ T.pack $ "List [" ++ show (length ls) ++ " elements]"
    ImGui.sameLine
    addClicked <- ImGui.button "+"

    when addClicked $ do
      writeIORef changedRef True

    -- Process each element with delete button
    resultRef <- newIORef ls

    ImGui.withIndent 5 $ do
      forM_ (zip [0..] ls) $ \(idx, val) -> do
        ImGui.withID idx $ do
          -- Delete button
          deleteClicked <- ImGui.button "X"
          when deleteClicked $ do
            modifyIORef resultRef (deleteAt idx)
            writeIORef changedRef True

          ImGui.sameLine
          ImGui.spacing  -- Add small space
          ImGui.sameLine
          ImGui.spacing  -- Add small space
          ImGui.sameLine

          -- Element widget
          (val', valChanged) <- ImGui.withIndent 15 $ widget val
          when valChanged $ do
            modifyIORef resultRef (updateAt idx val')
            writeIORef changedRef True

    result <- readIORef resultRef
    changed <- readIORef changedRef

    -- Add new element if button was clicked
    let finalResult = if addClicked then result ++ [def] else result

    return (finalResult, changed)
    where
      deleteAt :: Int -> [a] -> [a]
      deleteAt i xs = take i xs ++ drop (i + 1) xs

      updateAt :: Int -> a -> [a] -> [a]
      updateAt i x xs = take i xs ++ [x] ++ drop (i + 1) xs

instance (Default a, Default b, Widget a, Widget b) => Widget (a, b)

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
    
    c1 <- ImGui.colorEdit3 "##color" ref
    
    ImGui.ImVec3 newX newY newZ <- readIORef ref
    return (Color $ vec3 newX newY newZ, c1)

-- | A value between low and high
newtype InRange (low :: Nat) (high :: Nat) (a :: Type) = InRange { inRangeVal :: a }
  deriving (Eq, Ord, Show, Read, Num, Fractional, Real, Default)

-- Base type instances
instance (KnownNat low, KnownNat high) => Widget (InRange low high Int) where
  widget (InRange val) = do
    ref <- newIORef val
    changed <- ImGui.dragInt "##int" ref 1 (fromIntegral $ natVal (Proxy @low)) 
                                           (fromIntegral $ natVal (Proxy @high))
    newVal <- readIORef ref
    return (InRange newVal, changed)

instance (KnownNat low, KnownNat high) => Widget (InRange low high Float) where
  widget (InRange val) = do
    ref <- newIORef val
    changed <- ImGui.dragFloat "##float" ref 0.01 (fromIntegral $ natVal (Proxy @low)) 
                                                  (fromIntegral $ natVal (Proxy @high))
    newVal <- readIORef ref
    return (InRange newVal, changed)

instance (KnownNat low, KnownNat high) => Widget (InRange low high Double) where
  widget (InRange val) = do
    ref <- newIORef (realToFrac val)
    changed <- ImGui.dragFloat "##double" ref 0.01 (fromIntegral $ natVal (Proxy @low)) 
                                                   (fromIntegral $ natVal (Proxy @high))
    newVal <- readIORef ref
    return (InRange (realToFrac newVal), changed)

--------------------------------------------------------------------------------

-- | Collapsible section wrapper
-- When collapsed, the inner widget is not displayed
newtype Collapsible (lbl :: Symbol) a = Collapsible { unCollapsible :: a }
  deriving (Eq, Ord, Show, Default)

instance (KnownSymbol lbl, Widget a) => Widget (Collapsible lbl a) where
  widget (Collapsible val) = do
    isCurrentlyOpen <- ImGui.treeNode (T.pack $ symbolVal (Proxy @lbl) ++ "##collapsible")

    if isCurrentlyOpen
      then do
        ImGui.indent 5
        (newVal, changed) <- widget val
        ImGui.unindent 5
        ImGui.treePop
        return (Collapsible newVal, changed)
      else do
        return (Collapsible val, False)

-- | Checkbox wrapper
newtype Checkbox = Checkbox { checkboxValue :: Bool }
  deriving (Eq, Ord, Show, Default)

instance Widget Checkbox where
  widget (Checkbox val) = do
    ref <- newIORef val
    changed <- ImGui.checkbox "##checkbox" ref
    newVal <- readIORef ref
    return (Checkbox newVal, changed)

deriving via Checkbox instance Widget Bool

-- | Slider with explicit bounds (alternative to InRange for different UI)
newtype Slider (low :: Nat) (high :: Nat) (a :: Type) = Slider { sliderVal :: a }
  deriving (Eq, Ord, Show, Default)

instance (KnownNat low, KnownNat high) => Widget (Slider low high Float) where
  widget (Slider val) = do
    ref <- newIORef val
    changed <- ImGui.sliderFloat "##slider" ref 
                                 (fromIntegral $ natVal (Proxy @low))
                                 (fromIntegral $ natVal (Proxy @high))
    newVal <- readIORef ref
    return (Slider newVal, changed)

instance (KnownNat low, KnownNat high) => Widget (Slider low high Int) where
  widget (Slider val) = do
    ref <- newIORef val
    changed <- ImGui.sliderInt "##slider" ref
                               (fromIntegral $ natVal (Proxy @low))
                               (fromIntegral $ natVal (Proxy @high))
    newVal <- readIORef ref
    return (Slider newVal, changed)

-- | Tooltip wrapper - shows tooltip on hover
newtype WithTooltip (tip :: Symbol) a = WithTooltip { unTooltip :: a }
  deriving (Eq, Ord, Show, Default)

instance (KnownSymbol tip, Widget a) => Widget (WithTooltip tip a) where
  widget (WithTooltip content) = do
    (newContent, changed) <- widget content

    hover <- ImGui.isItemHovered
    when hover $ do
      ImGui.withTooltip $
        ImGui.text (T.pack $ symbolVal (Proxy @tip))

    return (WithTooltip newContent, changed)
