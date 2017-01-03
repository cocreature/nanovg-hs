{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module NanoVGSpec where

import           Contexts
import           Control.Applicative
import           Control.Monad
import qualified Data.Map as M
import           Data.Monoid
import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import           NanoVG
import           Test.Hspec
import           Test.QuickCheck

C.context (C.baseCtx <> nanoVGCtx)
C.include "nanovg.h"

arbitraryCFloat :: Gen CFloat
arbitraryCFloat = realToFrac <$> (arbitrary :: Gen Float)

arbitraryPtr :: Gen (Ptr a)
arbitraryPtr = ((nullPtr `plusPtr`) . getNonNegative) <$> (arbitrary :: Gen (NonNegative Int))

instance Arbitrary Transformation where
  arbitrary =
    do a <- arbitraryCFloat
       b <- arbitraryCFloat
       c <- arbitraryCFloat
       d <- arbitraryCFloat
       e <- arbitraryCFloat
       f <- arbitraryCFloat
       pure (Transformation
               (V2 (V3 a c e)
                   (V3 b d f)))

instance Arbitrary Extent where
  arbitrary =
    do a <- arbitraryCFloat
       b <- arbitraryCFloat
       pure (Extent (V2 a b))

instance Arbitrary Color where
  arbitrary =
    do r <- arbitraryCFloat
       g <- arbitraryCFloat
       b <- arbitraryCFloat
       a <- arbitraryCFloat
       pure (Color r g b a)                 

instance Arbitrary Paint where
  arbitrary =
    do xform <- arbitrary
       extent <- arbitrary
       radius <- arbitraryCFloat
       feather <- arbitraryCFloat
       innerColor <- arbitrary
       outerColor <- arbitrary
       image <- (Image . fromIntegral) <$> (arbitrary :: Gen Int)
       pure (Paint xform extent radius feather innerColor outerColor image)

instance Arbitrary TextRow where
  arbitrary =
    do start <- arbitraryPtr
       end <- arbitraryPtr
       next <- arbitraryPtr
       width <- arbitraryCFloat
       minx <- arbitraryCFloat
       maxx <- arbitraryCFloat
       pure (TextRow start end next width minx maxx)

instance Arbitrary GlyphPosition where
  arbitrary =
    do str <- arbitraryPtr
       x <- arbitraryCFloat
       minx <- arbitraryCFloat
       maxx <- arbitraryCFloat
       pure (GlyphPosition str x minx maxx)

instance Arbitrary Bounds where
  arbitrary = fmap Bounds $
    V4 <$> arbitraryCFloat
       <*> arbitraryCFloat
       <*> arbitraryCFloat
       <*> arbitraryCFloat

allocaElems :: forall a b. Storable a => Int -> (Ptr a -> IO b) -> IO b 
allocaElems n = allocaBytesAligned (n * sizeOf (undefined :: a)) (alignment (undefined :: a))

pokeElems :: Storable a => Ptr a -> [a] -> IO ()
pokeElems ptr as = forM_ (zip [0 ..] as) $ \(i,a) -> pokeElemOff ptr i a

peekElems :: Storable a => Int -> Ptr a -> IO [a]
peekElems n ptr = forM [0..(n-1)] (peekElemOff ptr)

roundTripElems :: (Storable a, Eq a, Show a) => [a] -> (CInt -> Ptr b -> Ptr b -> IO ()) -> IO ()
roundTripElems as f = allocaElems n $ \inPtr ->
                        allocaElems n $ \outPtr -> do
                          pokeElems inPtr as
                          let cInPtr = castPtr inPtr
                              cOutPtr = castPtr outPtr
                          f (fromIntegral n) cInPtr cOutPtr
                          peekElems (fromIntegral n) outPtr `shouldReturn` as
  where n = fromIntegral (length as)

roundTripTransformation :: [Transformation] -> IO ()
roundTripTransformation ts = do
  roundTripElems ts $ \n cInPtr cOutPtr ->
    [C.block| void {
      float* in = $(float* cInPtr);
      float* out = $(float* cOutPtr);
      for (int i = 0; i < $(int n); ++i) {
        for (int j = 0; j < 6; ++j) {
          out[6*i+j] = in[6*i+j];
        }
      }
    }|]

roundTripExtents :: [Extent] -> IO ()
roundTripExtents es = 
  roundTripElems es $ \n cInPtr cOutPtr ->
    [C.block| void {
    float* in = $(float* cInPtr);
    float* out = $(float* cOutPtr);
    for (int i = 0; i < $(int n); ++i) {
      out[2*i] = in[2*i];
      out[2*i+1] = in[2*i+1];
    }
    }|]

roundTripColors :: [Color] -> IO ()
roundTripColors cs =
  roundTripElems cs $ \n cInPtr cOutPtr ->
    [C.block| void {
    NVGcolor* in = $(NVGcolor* cInPtr);
    NVGcolor* out = $(NVGcolor* cOutPtr);
    for (int i = 0; i < $(int n); ++i) {
      out[i].r = in[i].r;
      out[i].g = in[i].g;
      out[i].b = in[i].b;
      out[i].a = in[i].a;
    }
    }|]

roundTripPaints :: [Paint] -> IO ()
roundTripPaints ps = 
  roundTripElems ps $ \n cInPtr cOutPtr -> 
    [C.block| void {
       NVGpaint* in = $(NVGpaint* cInPtr);
       NVGpaint* out = $(NVGpaint* cOutPtr);
       for (int i = 0; i < $(int n); ++i) {
           for (int j = 0; j < 6; ++j) {
             out[i].xform[j] = in[i].xform[j];
           }
           out[i].extent[0] = in[i].extent[0];
           out[i].extent[1] = in[i].extent[1];
           out[i].radius = in[i].radius;
           out[i].feather = in[i].feather;
           out[i].innerColor = in[i].innerColor;
           out[i].outerColor = in[i].outerColor;
           out[i].image = in[i].image;
       }
     }|]

roundTripGlyphPositions :: [GlyphPosition] -> IO ()
roundTripGlyphPositions ps =
  roundTripElems ps $ \n cInPtr cOutPtr -> 
    [C.block| void {
      NVGglyphPosition* in = $(NVGglyphPosition* cInPtr);
      NVGglyphPosition* out = $(NVGglyphPosition* cOutPtr);
      for (int i = 0; i < $(int n); ++i) {
        out[i].str = in[i].str;
        out[i].x = in[i].x;
        out[i].minx = in[i].minx;
        out[i].maxx = in[i].maxx;
      }     
    }|]

roundTripTextRows :: [TextRow] -> IO ()
roundTripTextRows rs =
  roundTripElems rs $ \n cInPtr cOutPtr ->
    [C.block| void {
      NVGtextRow* in = $(NVGtextRow* cInPtr);
      NVGtextRow* out = $(NVGtextRow* cOutPtr);
      for (int i = 0; i < $(int n); ++i) {
        out[i].start = in[i].start;
        out[i].end = in[i].end;
        out[i].next = in[i].next;
        out[i].width = in[i].width;
        out[i].minx = in[i].minx;
        out[i].maxx = in[i].maxx;
      }
    }|]

roundTripBounds :: [Bounds] -> IO ()
roundTripBounds bs =
  roundTripElems bs $ \n cInPtr cOutPtr ->
    [C.block| void {
      float* in = $(float* cInPtr);
      float* out = $(float* cOutPtr);
      for (int i = 0; i < $(int n); ++i) {
        for (int j = 0; j < 4; ++j) {
          out[4*i+j] = in[4*i+j];
        }
      }
    }|]

spec :: Spec
spec = do
  describe "Storable Transformation" $ do
    it "roundtrips via the storable instance" $ do
      quickCheck roundTripTransformation
  describe "Storable Extent" $ do
    it "roundtrips via the storable instance" $ do
      quickCheck roundTripExtents
  describe "Storable Color" $ do
    it "roundtrips via the storable instance" $ do
      quickCheck roundTripColors
  describe "Storable Paint" $ do
    it "roundtrips via the storable instance" $ do
      quickCheck roundTripPaints
  describe "Storable GlyphPosition" $ do
    it "roundtrips via the storable instance" $ do
      quickCheck roundTripGlyphPositions
  describe "Storable TextRow" $ do
    it "roundtrips via the storable instance" $ do
      quickCheck roundTripTextRows
  describe "Storable Bounds" $ do
    it "roundtrips via the storable instance" $ do
      quickCheck roundTripBounds
