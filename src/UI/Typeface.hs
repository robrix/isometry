{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module UI.Typeface
( Typeface(name, glyphs)
, Font(..)
, fontScale
, metrics
, readTypeface
, readFontOfSize
, cacheCharactersForDrawing
, layoutString
) where

import           Control.Effect.Finally
import           Control.Effect.Trace
import           Control.Lens
import           Control.Monad (guard, join, (<=<))
import           Control.Monad.IO.Class.Lift
import           Data.Bifunctor (first)
import           Data.Char (isPrint, isSeparator, ord)
import           Data.Coerce (coerce)
import           Data.Foldable (find, foldl')
import           Data.Functor.I (I(..))
import           Data.Functor.Interval (Interval(..))
import           Data.IORef
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Vector ((!?))
import           Geometry.Triangle
import           GHC.Stack
import           GL.Array
import           GL.Buffer as B
import           GL.Effect.Check
import           GL.Object
import           GL.Program
import           Linear.V2
import qualified Opentype.Fileformat as O
import           UI.Drawable
import           UI.Glyph
import qualified UI.Label.Glyph as Glyph
import           UI.Path

data Typeface = Typeface
  { name         :: Maybe String
  , allGlyphs    :: Map.Map Char (Maybe Glyph)
  , opentypeFont :: O.OpentypeFont
  , glyphs       :: Drawable Glyph.U Glyph.V Glyph.Frag
  , glyphsB      :: Buffer 'B.Array (Glyph.V  I)
  , rangesRef    :: IORef (Map.Map Char (Interval I Int))
  }

data Font = Font
  { face :: Typeface
  , size :: Float
  }

fontScale :: Font -> Float
fontScale (Font face size) = size * scale where
  scale = 1 / fromIntegral (O.unitsPerEm (O.headTable (opentypeFont face)))

metrics :: Font -> (Float, Float)
metrics font = (ascent, descent) where
  table = O.hheaTable (opentypeFont (face font))
  scale = fontScale font
  ascent = scale * fromIntegral (O.ascent table)
  descent = scale * fromIntegral (O.descent table)


readTypeface
  :: ( HasCallStack
     , Has Check sig m
     , Has Finally sig m
     , Has (Lift IO) sig m
     )
  => FilePath
  -> m Typeface
readTypeface = fromOpentypeFont <=< sendM . O.readOTFile

fromOpentypeFont
  :: ( HasCallStack
     , Has Check sig m
     , Has Finally sig m
     , Has (Lift IO) sig m
     )
  => O.OpentypeFont
  -> m Typeface
fromOpentypeFont opentypeFont = do
  program <- build Glyph.shader
  array   <- gen1
  glyphsB <- gen1

  rangesRef <- sendM (newIORef Map.empty)

  pure Typeface
    { name
    , allGlyphs
    , opentypeFont
    , glyphs = Drawable{ program, array }
    , glyphsB
    , rangesRef
    } where
  name = T.unpack . T.decodeUtf16BE . O.nameString <$> find ((== Just FullName) . nameID) (O.nameRecords (O.nameTable opentypeFont))
  allGlyphs = Map.fromList (map ((,) <*> lookupGlyph) [minBound..maxBound])
  lookupGlyph char = do
    table <- O.glyphMap <$> cmap
    glyphID <- table Map.!? fromIntegral (ord char)
    glyphTable <- glyphTable
    g <- glyphTable !? fromIntegral glyphID
    let vertices = if isPrint char && not (isSeparator char) then glyphVertices g else []
    pure $! Glyph char (fromIntegral (O.advanceWidth g)) vertices (bounds (map (^._xy) vertices))
  cmap = find supportedPlatform (O.getCmaps (O.cmapTable opentypeFont))
  glyphTable = case O.outlineTables opentypeFont of
    O.QuadTables _ (O.GlyfTable glyphs) -> Just glyphs
    _                                   -> Nothing
  supportedPlatform p = O.cmapPlatform p == O.UnicodePlatform || O.cmapPlatform p == O.MicrosoftPlatform && O.cmapEncoding p == 1
  glyphVertices = uncurry triangleVertices . first (fmap fromIntegral) <=< pathTriangles <=< map contourToPath . O.getScaledContours opentypeFont

readFontOfSize
  :: ( HasCallStack
     , Has Check sig m
     , Has Finally sig m
     , Has (Lift IO) sig m
     )
  => FilePath
  -> Float
  -> m Font
readFontOfSize path size = (`Font` size) <$> readTypeface path


cacheCharactersForDrawing :: (Effect sig, Has Check sig m, Has (Lift IO) sig m, Has Trace sig m) => Typeface -> String -> m ()
cacheCharactersForDrawing Typeface{ allGlyphs, glyphs = Drawable { array }, glyphsB, rangesRef } string = do
  let (vs, ranges, _) = foldl' combine (id, Map.empty, 0) (glyphsForString allGlyphs string)
      combine (vs, cs, i) Glyph{ char, geometry } = let i' = i + I (length geometry) in (vs . (geometry ++), Map.insert char (Interval i i') cs, i')
      vertices = vs []

  bindArray array $
    bindBuffer glyphsB $ do
      realloc @'B.Array (length vertices) Static Draw
      copy @'B.Array 0 (coerce (map (fmap (fromIntegral @_ @Float)) vertices))

      configureInterleaved

  sendM (writeIORef rangesRef ranges)


data NameID
  = Copyright
  | FamilyName
  | SubfamilyName
  | UniqueID
  | FullName
  | Version
  | PostScriptName
  | Trademark
  | ManufacturerName
  | Designer
  | Description
  | VendorURL
  | DesignerURL
  | LicenseDescription
  | LicenseURL
  | Reserved
  | TypographicFamilyName
  | TypographicSubfamilyName
  | CompatibleFullName
  | SampleText
  | PostScriptCIDFindFontName
  | WWSFamilyName
  | WWSSubfamilyName
  | LightBackgroundPalette
  | DarkBackgroundPalette
  | VariationsPostScriptNamePrefix
  deriving (Bounded, Enum, Eq, Ord, Show)

nameID :: O.NameRecord -> Maybe NameID
nameID = safeToEnum . fromIntegral . O.nameID


safeToEnum :: forall n. (Bounded n, Enum n) => Int -> Maybe n
safeToEnum n = toEnum n <$ guard (n < fromEnum (maxBound @n) && n > fromEnum (minBound @n))


layoutString :: Has (Lift IO) sig m => Typeface -> String -> m Run
layoutString face string = do
  ranges <- sendM (readIORef (rangesRef face))
  pure (layoutGlyphs ranges (glyphsForString (allGlyphs face) string))

glyphsForString :: Map.Map Char (Maybe Glyph) -> String -> [Glyph]
glyphsForString allGlyphs = mapMaybe (join . (allGlyphs Map.!?))


contourToPath :: [O.CurvePoint] -> Path V2 O.FWord
contourToPath [] = []
contourToPath (p@(O.CurvePoint x y _) : ps) = M (V2 x y) : go p ps where
  go (O.CurvePoint _  _  True)  (p@(O.CurvePoint _  _  False) : ps) = go p ps
  go (O.CurvePoint _  _  True)  (p@(O.CurvePoint x  y  True)  : ps) = L (V2 x y) : go p ps
  go (O.CurvePoint x1 y1 False) (p@(O.CurvePoint x2 y2 False) : ps) = Q (V2 x1 y1) (V2 (x1 + ((x2 - x1) `div` 2)) (y1 + ((y2 - y1) `div` 2))) : go p ps
  go (O.CurvePoint x1 y1 False) (p@(O.CurvePoint x2 y2 True)  : ps) = Q (V2 x1 y1) (V2 x2 y2) : go p ps
  go (O.CurvePoint x1 y1 False) []                                  = [ Q (V2 x1 y1) (V2 x y) ]
  go _                          []                                  = []
