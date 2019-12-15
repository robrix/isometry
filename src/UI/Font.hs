{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module UI.Font
( Typeface(name)
, Font(..)
, readTypeface
, readFontOfSize
, unitsPerEm
, ascent
, descent
, glyphs
) where

import Control.Monad ((<=<), guard)
import Control.Monad.IO.Class.Lift
import Data.Bifunctor (first)
import Data.Char (ord)
import Data.Foldable (find)
import Data.Int
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Vector (Vector, (!?))
import Data.Word
import Geometry.Triangle
import qualified Opentype.Fileformat as O
import Lens.Micro
import Linear.V2
import Linear.V4
import Linear.Vector ((*^), (^/))
import UI.Glyph
import UI.Path

data Typeface = Typeface { name :: Maybe String, _font :: O.OpentypeFont }

data Font = Font { face :: Typeface, size :: Float }


readTypeface :: Has (Lift IO) sig m => FilePath -> m Typeface
readTypeface path = sendM ((Typeface . opentypeFontName <*> id) <$> O.readOTFile path)

readFontOfSize :: Has (Lift IO) sig m => FilePath -> Float -> m Font
readFontOfSize path size = (`Font` size) <$> readTypeface path

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

opentypeFontName :: O.OpentypeFont -> Maybe String
opentypeFontName o = T.unpack . T.decodeUtf16BE . O.nameString <$> find ((== Just FullName) . nameID) (O.nameRecords (O.nameTable o))

nameID :: O.NameRecord -> Maybe NameID
nameID = safeToEnum . fromIntegral . O.nameID


safeToEnum :: forall n. (Bounded n, Enum n) => Int -> Maybe n
safeToEnum n = toEnum n <$ guard (n < fromEnum (maxBound @n) && n > fromEnum (minBound @n))


unitsPerEm :: Typeface -> Word16
unitsPerEm = O.unitsPerEm . O.headTable . _font

ascent :: Typeface -> Int16
ascent = O.ascent . O.hheaTable . _font

descent :: Typeface -> Int16
descent = O.descent . O.hheaTable . _font

glyphTable :: Typeface -> Vector (O.Glyph Int)
glyphTable (Typeface _ o) = case O.outlineTables o of
  O.QuadTables _ (O.GlyfTable glyphs) -> glyphs
  _ -> error "wtf"

supportedCMap :: Typeface -> Maybe O.CMap
supportedCMap = find supportedPlatform . O.getCmaps . O.cmapTable . _font
  where supportedPlatform p = O.cmapPlatform p == O.UnicodePlatform || O.cmapPlatform p == O.MicrosoftPlatform && O.cmapEncoding p == 1


glyphs :: Font -> [Char] -> [Glyph]
glyphs (Font face size) chars = catMaybes (zipWith (fmap . toGlyph) chars (glyphsForChars face chars))
  where toGlyph char g = let vertices = glyphVertices face g in
          scaleGlyph (size *^ scale) $ Glyph char (fromIntegral (O.advanceWidth g)) vertices (bounds (map (^. _xy) vertices))
        scale = 1 ^/ fromIntegral (unitsPerEm face)

glyphsForChars :: Typeface -> [Char] -> [Maybe (O.Glyph Int)]
glyphsForChars face = map lookupGlyph
  where lookupGlyph char = do
          table <- O.glyphMap <$> cmap
          glyphID <- table Map.!? fromIntegral (ord char)
          glyphTable face !? fromIntegral glyphID
        cmap = supportedCMap face


contourToPath :: [O.CurvePoint] -> Path V2 O.FWord
contourToPath [] = []
contourToPath (p@(O.CurvePoint x y _) : ps) = M (V2 x y) : go p ps
  where go (O.CurvePoint _  _  True)  (p@(O.CurvePoint _  _  False) : ps) = go p ps
        go (O.CurvePoint _  _  True)  (p@(O.CurvePoint x  y  True)  : ps) = L (V2 x y) : go p ps
        go (O.CurvePoint x1 y1 False) (p@(O.CurvePoint x2 y2 False) : ps) = Q (V2 x1 y1) (V2 (x1 + ((x2 - x1) `div` 2)) (y1 + ((y2 - y1) `div` 2))) : go p ps
        go (O.CurvePoint x1 y1 False) (p@(O.CurvePoint x2 y2 True)  : ps) = Q (V2 x1 y1) (V2 x2 y2) : go p ps
        go (O.CurvePoint x1 y1 False) []                                  = Q (V2 x1 y1) (V2 x y) : []
        go _                          []                                  = []

glyphPaths :: Typeface -> O.Glyph Int -> [Path V2 O.FWord]
glyphPaths typeface glyph = fmap contourToPath (O.getScaledContours (_font typeface) glyph)


glyphVertices :: Typeface -> O.Glyph Int -> [V4 Float]
glyphVertices typeface = uncurry triangleVertices . first (fmap fromIntegral) <=< pathTriangles <=< glyphPaths typeface
