{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module NanoVG.Internal.Text where

import           Control.Applicative (pure)
import           Data.ByteString hiding (null)
import qualified Data.Set as S
import qualified Data.Text as T
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           NanoVG.Internal.Context
import           NanoVG.Internal.FFIHelpers
import           NanoVG.Internal.FixedVector
import           NanoVG.Internal.Types
import           Prelude hiding (null)

#include "nanovg.h"

{#pointer *NVGcontext as Context newtype nocode#}

{#enum NVGalign as Align 
         {underscoreToCase} with prefix = "NVG_"
         deriving (Show,Read,Eq,Ord)#}

-- | Newtype to avoid accidental use of ints
newtype Font = Font {fontHandle :: CInt} deriving (Show,Read,Eq,Ord)

data TextRow =
  TextRow { -- | Pointer to the input text where the row starts.
            start :: !(Ptr CChar)
            -- | Pointer to the input text where the row ends (one past the last character).
          , end :: !(Ptr CChar)
            -- | Pointer to the beginning of the next row.
          , next :: !(Ptr CChar)
            -- | Logical width of the row.
          , width :: !CFloat
            -- | Actual bounds of the row. Logical with and bounds can differ because of kerning and some parts over extending.
          , textRowMinX :: !CFloat
          , textRowMaxX :: !CFloat}
  deriving (Show,Eq,Ord)

instance Storable TextRow where
  sizeOf _ = 40
  alignment _ = {#alignof NVGtextRow#}
  peek p =
    do start <- {#get NVGtextRow->start#} p
       end <- {#get NVGtextRow->end#} p
       next <- {#get NVGtextRow->next#} p
       width <- {#get NVGtextRow->width#} p
       minX <- {#get NVGtextRow->minx#} p
       maxX <- {#get NVGtextRow->maxx#} p
       pure (TextRow start end next width minX maxX)
  poke p (TextRow {..}) =
    do {#set NVGtextRow->start#} p start
       {#set NVGtextRow->end#} p end
       {#set NVGtextRow->next#} p next
       {#set NVGtextRow->width#} p width
       {#set NVGtextRow->minx#} p textRowMinX
       {#set NVGtextRow->maxx#} p textRowMaxX

{#pointer *NVGtextRow as TextRowPtr -> TextRow#}

data GlyphPosition =
     GlyphPosition { -- | Pointer of the glyph in the input string.
                     str :: !(Ptr CChar)
                     -- | The x-coordinate of the logical glyph position.
                   , glyphX :: !CFloat
                     -- | The left bound of the glyph shape.
                   , glyphPosMinX :: !CFloat
                     -- | The right bound of the glyph shape.
                   , glyphPosMaxX :: !CFloat} deriving (Show,Eq,Ord)

instance Storable GlyphPosition where
  sizeOf _ = 24
  alignment _ = {#alignof NVGglyphPosition#}
  peek p =
    do str <- {#get NVGglyphPosition->str#} p
       x <- {#get NVGglyphPosition->x#} p
       minx <- {#get NVGglyphPosition->minx#} p
       maxx <- {#get NVGglyphPosition->maxx#} p
       pure (GlyphPosition str x minx maxx)
  poke p (GlyphPosition str x minx maxx) =
    do {#set NVGglyphPosition->str#} p str
       {#set NVGglyphPosition->x#} p x
       {#set NVGglyphPosition->minx#} p minx
       {#set NVGglyphPosition->maxx#} p maxx

{#pointer *NVGglyphPosition as GlyphPositionPtr -> GlyphPosition#}

safeFont :: CInt -> Maybe Font
safeFont i
  | i < 0 = Nothing
  | otherwise = Just (Font i)

-- | Creates font by loading it from the disk from specified file name.
-- Returns handle to the font.
{#fun unsafe nvgCreateFont as createFont
        {`Context',withCString*`T.Text','withCString.unwrapFileName'*`FileName'} -> `Maybe Font'safeFont#}

-- | Creates font by loading it from the disk from specified file name.
-- fontIndex specifies which font face to load from a .ttf/.ttc file.
-- Returns handle to the font.
{#fun unsafe nvgCreateFontAtIndex as createFontAtIndex
        {`Context',withCString*`T.Text','withCString.unwrapFileName'*`FileName', `CInt'} -> `Maybe Font'safeFont#}

-- | Creates font by loading it from the specified memory chunk.
-- Returns handle to the font.
{#fun unsafe nvgCreateFontMem as createFontMem
        {`Context',withCString*`T.Text',useAsCStringLen'*`ByteString'&,one-`CInt'} -> `Maybe Font'safeFont#}

-- | Creates font by loading it from the specified memory chunk.
-- fontIndex specifies which font face to load from a .ttf/.ttc file.
-- Returns handle to the font.
{#fun unsafe nvgCreateFontMemAtIndex as createFontMemAtIndex
        {`Context',withCString*`T.Text',useAsCStringLen'*`ByteString'&,one-`CInt',`CInt'} -> `Maybe Font'safeFont#}

-- | Finds a loaded font of specified name, and returns handle to it, or -1 if the font is not found.
{#fun unsafe nvgFindFont as findFont
        {`Context', withCString*`T.Text'} -> `Maybe Font'safeFont#}

-- | Adds a fallback font by handle.
{#fun unsafe nvgAddFallbackFontId as addFallbackFontId
        {`Context',`CInt',`CInt'} -> `Maybe Font'safeFont#}

-- | Adds a fallback font by name.
{#fun unsafe nvgAddFallbackFont as addFallbackFont
        {`Context',withCString*`T.Text',withCString*`T.Text'} -> `Maybe Font'safeFont#}

-- | Resets fallback fonts by handle.
{#fun unsafe nvgResetFallbackFontsId as resetFallbackFontsId
        {`Context',`CInt'} -> `()'#}

-- | Resets fallback fonts by name.
{#fun unsafe nvgResetFallbackFonts as resetFallbackFonts
        {`Context',withCString*`T.Text'} -> `()'#}

-- | Sets the font size of current text style.
{#fun unsafe nvgFontSize as fontSize
        {`Context',`CFloat'} -> `()'#}

-- | Sets the blur of current text style.
{#fun unsafe nvgFontBlur as fontBlur
        {`Context',`CFloat'} -> `()'#}

-- | Sets the letter spacing of current text style.
{#fun unsafe nvgTextLetterSpacing as textLetterSpacing
        {`Context',`CFloat'} -> `()'#}

-- | Sets the proportional line height of current text style. The line height is specified as multiple of font size. 
{#fun unsafe nvgTextLineHeight as textLineHeight
        {`Context',`CFloat'} -> `()'#}

-- | Sets the text align of current text style, see NVGalign for options.
{#fun unsafe nvgTextAlign as textAlign
        {`Context',bitMask`S.Set Align'} -> `()'#}

-- | Sets the font face based on specified id of current text style.
{#fun unsafe nvgFontFaceId as fontFaceId
        {`Context',fontHandle`Font'} -> `()'#}

-- | Sets the font face based on specified name of current text styl
{#fun unsafe nvgFontFace as fontFace
        {`Context',withCString*`T.Text'} -> `()'#}

-- | Draws text string at specified location. If end is specified only the sub-string up to the end is drawn.
{#fun unsafe nvgText as text
         {`Context',`CFloat',`CFloat',id`Ptr CChar',id`Ptr CChar'} -> `()'#}

-- | Draws multi-line text string at specified location wrapped at the specified width. If end is specified only the sub-string up to the end is drawn.
-- | White space is stripped at the beginning of the rows, the text is split at word boundaries or when new-line characters are encountered.
-- | Words longer than the max width are slit at nearest character (i.e. no hyphenation).
{#fun unsafe nvgTextBox as textBox
        {`Context',`CFloat',`CFloat',`CFloat',withCString*`T.Text',null-`Ptr CUChar'} -> `()'#}

newtype Bounds = Bounds (V4 CFloat) deriving (Show,Read,Eq,Ord)

instance Storable Bounds where
  sizeOf _ = sizeOf (0 :: CFloat) * 4
  alignment _ = alignment (0 :: CFloat)
  peek p =
    do let p' = castPtr p :: Ptr CFloat
       a <- peekElemOff p' 0
       b <- peekElemOff p' 1
       c <- peekElemOff p' 2
       d <- peekElemOff p' 3
       pure (Bounds (V4 a b c d))
  poke p (Bounds (V4 a b c d)) =
    do let p' = castPtr p :: Ptr CFloat
       pokeElemOff p' 0 a
       pokeElemOff p' 1 b
       pokeElemOff p' 2 c
       pokeElemOff p' 3 d

peekBounds :: Ptr CFloat -> IO Bounds
peekBounds = peek . castPtr

allocaBounds :: (Ptr CFloat -> IO b) -> IO b
allocaBounds f = alloca (\(p :: Ptr Bounds) -> f (castPtr p))

-- | Measures the specified text string. Parameter bounds should be a pointer to float[4],
-- if the bounding box of the text should be returned. The bounds value are [xmin,ymin, xmax,ymax]
-- Returns the horizontal advance of the measured text (i.e. where the next character should drawn).
-- Measured values are returned in local coordinate space.
{#fun unsafe nvgTextBounds as textBounds
        {`Context',`CFloat',`CFloat',withCString*`T.Text',null-`Ptr CUChar', allocaBounds-`Bounds'peekBounds*} -> `()'#}

-- | Measures the specified multi-text string. Parameter bounds should be a pointer to float[4],
-- if the bounding box of the text should be returned. The bounds value are [xmin,ymin, xmax,ymax]
-- Measured values are returned in local coordinate space.
{#fun unsafe nvgTextBoxBounds as textBoxBounds
        {`Context',`CFloat',`CFloat',`CFloat',withCString*`T.Text',null-`Ptr CUChar',allocaBounds-`Bounds'peekBounds*} -> `()'#}

-- | Calculates the glyph x positions of the specified text. If end is specified only the sub-string will be used.
-- Measured values are returned in local coordinate space.
{#fun unsafe nvgTextGlyphPositions as textGlyphPositions
        {`Context',`CFloat',`CFloat',id`Ptr CChar',id`Ptr CChar',`GlyphPositionPtr', `CInt'} -> `CInt'#}

-- | Returns the vertical metrics based on the current text style.
-- Measured values are returned in local coordinate space.
{#fun unsafe nvgTextMetrics as textMetrics
        {`Context',alloca-`CFloat'peek*,alloca-`CFloat'peek*,alloca-`CFloat'peek*} -> `()'#}

-- | Breaks the specified text into lines. If end is specified only the sub-string will be used.
-- White space is stripped at the beginning of the rows, the text is split at word boundaries or when new-line characters are encountered.
-- Words longer than the max width are slit at nearest character (i.e. no hyphenation).
{#fun unsafe nvgTextBreakLines as textBreakLines
        {`Context',id`Ptr CChar',id`Ptr CChar',`CFloat',`TextRowPtr',`CInt'} -> `CInt'#}
