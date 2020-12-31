{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative (pure)
import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.Trans.Maybe
import           Data.Bits hiding (rotate)
import           Data.IORef
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V
import           Graphics.GL.Core32
import           Graphics.UI.GLFW hiding (Image)
import           NanoVG as NVG
import           NanoVG.Internal.Text as Internal
import           Prelude hiding (init)

import           Foreign.C.Types
import           Foreign.Ptr

foreign import ccall unsafe "initGlew"
  glewInit :: IO CInt

main :: IO ()
main =
  do e <- init
     when (not e) $ putStrLn "Failed to init GLFW"
     windowHint $ WindowHint'ContextVersionMajor 3
     windowHint $ WindowHint'ContextVersionMinor 2
     windowHint $ WindowHint'OpenGLForwardCompat True
     windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core
     windowHint $ WindowHint'OpenGLDebugContext True
     win <- createWindow 1000 600 "NanoVG" Nothing Nothing
     case win of
       Nothing -> putStrLn "Failed to create window" >> terminate
       Just w ->
         do makeContextCurrent win
            glewInit
            glGetError
            c@(Context c') <- createGL3 (S.fromList [Antialias,StencilStrokes,Debug])
            -- error handling? who needs that anyway
            Just demoData <- runMaybeT $ loadDemoData c
            swapInterval 0
            setTime 0
            whileM_ (not <$> windowShouldClose w) $
              do Just t <- getTime
                 (mx,my) <- getCursorPos w
                 (width,height) <- getWindowSize w
                 (fbWidth,fbHeight) <- getFramebufferSize w
                 let pxRatio = fromIntegral fbWidth / fromIntegral width
                 glViewport 0 0 (fromIntegral fbWidth) (fromIntegral fbHeight)
                 glClearColor 0.3 0.3 0.32 1.0
                 glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT)
                 beginFrame c (fromIntegral width) (fromIntegral height) pxRatio
                 renderDemo c demoData mx my width height t
                 endFrame c
                 swapBuffers w
                 pollEvents

renderDemo :: Context -> DemoData -> Double -> Double -> Int -> Int -> Double -> IO ()
renderDemo c demoData mx my w h t =
  do drawEyes c (fromIntegral w - 250) 50 150 100 (realToFrac mx) (realToFrac my) (realToFrac t)
     drawParagraph c (fromIntegral w - 450) 50 150 100 (realToFrac mx) (realToFrac my)
     drawGraph c 0 (fromIntegral h/2) (fromIntegral w) (fromIntegral h/2) (realToFrac t)
     drawColorwheel c (fromIntegral w - 300) (fromIntegral h - 300) 250 250 (realToFrac t)

     drawLines c 120 (fromIntegral h - 50) 600 50 (realToFrac t)

     drawWidths c 10 50 30

     drawCaps c 10 300 30

     drawScissor c 50 (fromIntegral h-80) (realToFrac t)

     drawComposite c

     save c
     let popy = 95 + 24
     drawThumbnails c 365 popy 160 300 (images demoData) (realToFrac t)

     restore c

drawThumbnails :: Context -> CFloat -> CFloat -> CFloat -> CFloat -> V.Vector Image -> CFloat -> IO ()
drawThumbnails vg x y w h images t =
  do let cornerRadius = 3
         thumb = 60
         arry = 30.5
         nimages = V.length images
         stackh = (fromIntegral nimages/2)*(thumb+10)+10
         u = (1+cos (t*0.5))*0.5
         u2 = (1-cos (t*0.2))*0.5
         dv = 1/(fromIntegral nimages - 1)
     save vg

     shadowPaint <- boxGradient vg x (y+4) w h (cornerRadius*2) 20 (rgba 0 0 0 128) (rgba 0 0 0 0)
     beginPath vg
     rect vg (x-10) (y-10) (w+20) (h+30)
     roundedRect vg x y w h cornerRadius
     pathWinding vg (fromIntegral $ fromEnum Hole)
     fillPaint vg shadowPaint
     fill vg

     beginPath vg
     roundedRect vg x y w h cornerRadius
     moveTo vg (x-10) (y+arry)
     lineTo vg (x+1) (y+arry-11)
     lineTo vg (x+1) (y+arry+11)
     fillColor vg (rgba 200 200 200 255)
     fill vg

     save vg
     scissor vg x y w h
     translate vg 0 (-(stackh-h)*u)

     flip V.imapM_ images $ \i image -> do
       let tx = x + 10 + fromIntegral (i `mod` 2) * (thumb + 10)
           ty = y + 10 + fromIntegral (i `div` 2) * (thumb + 10)
           v = fromIntegral i * dv
           a = clamp ((u2-v)/dv) 0 1
           drawImage iw ih ix iy = do
             imgPaint <- imagePattern vg (tx+ix) (ty+iy) iw ih (0/180*pi) image a
             beginPath vg
             roundedRect vg tx ty thumb thumb 5
             fillPaint vg imgPaint
             fill vg
       (imgw,imgh) <- imageSize vg image

       when (a < 1) $ drawSpinner vg (tx + thumb/2) (ty+thumb/2) (thumb*0.25) t

       if imgw < imgh
       then
         let iw = thumb
             ih = iw*fromIntegral imgh/fromIntegral imgw
             ix = 0
             iy = -(ih-thumb)*0.5
         in drawImage iw ih ix iy
       else
         let ih = thumb
             iw = ih * fromIntegral imgw/ fromIntegral imgh
             ix = -(iw-thumb)*0.5
             iy = 0
         in drawImage iw ih ix iy

       shadowPaint <- boxGradient vg (tx-1) ty (thumb+2) (thumb+2) 5 3 (rgba 0 0 0 128) (rgba 0 0 0 0)
       beginPath vg
       rect vg (tx-5) (ty-5) (thumb+10) (thumb+10)
       roundedRect vg tx ty thumb thumb 6
       pathWinding vg (fromIntegral $ fromEnum Hole)
       fillPaint vg shadowPaint
       fill vg

       beginPath vg
       roundedRect vg (tx+0.5) (ty+0.5) (thumb-1) (thumb-1) (4-0.5)
       strokeWidth vg 1
       strokeColor vg (rgba 255 255 255 192)
       stroke vg


     restore vg

     restore vg

drawSpinner :: Context -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()
drawSpinner vg cx cy r t =
  do let a0 = 0+t*6
         a1 = pi + t*6
         r0 = r
         r1 = r*0.75
     save vg

     beginPath vg
     arc vg cx cy r0 a0 a1 CW
     arc vg cx cy r1 a1 a0 CCW
     closePath vg
     let ax = cx+cos a0 * (r0+r1)*0.5
         ay = cy+sin a0 * (r0+r1)*0.5
         bx = cx+cos a1 * (r0+r1)*0.5
         by = cy+sin a1 * (r0+r1)*0.5
     paint <- linearGradient vg ax ay bx by (rgba 0 0 0 0) (rgba 0 0 0 128)
     fillPaint vg paint
     fill vg

     restore vg

drawEyes :: Context -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()
drawEyes c@(Context c') x y w h mx my t = do
  bg <- linearGradient c x (y+h*0.5) (x+w*0.1) (y+h) (rgba 0 0 0 32) (rgba 0 0 0 16)
  beginPath c
  ellipse c (lx+3) (ly+16) ex ey
  ellipse c (rx+3) (ry+16) ex ey
  fillPaint c bg
  fill c

  bg <- linearGradient c x (y+h*0.25) (x+w*0.1) (y+h) (rgba 220 220 220 255) (rgba 128 128 128 255)
  beginPath c
  ellipse c lx ly ex ey
  ellipse c rx ry ex ey
  fillPaint c bg
  fill c
  
  let dx' = (mx - rx) / (ex * 10)
      dy' = (my - ry) / (ey * 10)
      d = sqrt (dx'*dx'+dy'*dy')
      dx'' = if d > 1 then dx'/d else dx'
      dy'' = if d > 1 then dy'/d else dy'
      dx = dx'' * ex * 0.4
      dy = dy'' * ey * 0.5
  beginPath c
  ellipse c (lx+dx) (ly+dy+ey*0.25*(1-blink)) br (br*blink)
  fillColor c (rgba 32 32 32 255)
  fill c

  let dx'' = (mx - rx) / (ex * 10)
      dy'' = (my - ry) / (ey * 10)
      d = sqrt (dx'' * dx'' + dy'' * dy'')
      dx' = if d > 1 then dx'' / d else dx''
      dy' = if d > 1 then dy'' / d else dy''
      dx = dx' * ex * 0.4
      dy = dy' * ey * 0.5
  beginPath c
  ellipse c (rx+dx) (ry+dy+ey*0.25*(1-blink)) br (br*blink)
  fillColor c (rgba 32 32 32 255)
  fill c

  gloss <- radialGradient c (lx-ex*0.25) (ly-ey*0.5) (ex*0.1) (ex*0.75) (rgba 255 255 255 128) (rgba 255 255 255 0)
  beginPath c
  ellipse c lx ly ex ey
  fillPaint c gloss
  fill c

  gloss <- radialGradient c (rx-ex*0.25) (ry-ey*0.5) (ex*0.1) (ex*0.75) (rgba 255 255 255 128) (rgba 255 255 255 0)
  beginPath c
  ellipse c rx ry ex ey
  fillPaint c gloss
  fill c
  where ex = w * 0.23
        ey = h * 0.5
        lx = x + ex
        ly = y + ey
        rx = x + w - ex
        ry = y + ey
        br = 0.5 * min ex ey
        blink = 1 - ((sin (t*0.5))**200)*0.8

drawParagraph :: Context -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()
drawParagraph c x y w h mx my =
  do save c
     fontSize c 18
     fontFace c "sans"
     textAlign c (S.fromList [AlignLeft,AlignTop])
     (_,_,lineh) <- textMetrics c
     gutter <- newIORef Nothing
     yEnd <- newIORef y
     NVG.textBreakLines c text w 3 $ \row i -> do
       let y' = y + fromIntegral i * lineh
           hit = mx > x && mx < (x+w) && my >= y' && my < (y' + lineh)
       writeIORef yEnd y'
       beginPath c
       fillColor c (rgba 255 255 255 (if hit then 64 else 16))
       rect c x y' (width row) lineh
       fill c

       fillColor c (rgba 255 255 255 255)
       Internal.text c x y' (start row) (end row)
       when hit $ do
         let caretxInit = if mx < x+ (width row) / 2 then x else x + width row
             ps = x
         glyphs <- NVG.textGlyphPositions c x y (start row) (end row) 100
         let leftBorders = V.map glyphX glyphs
             rightBorders = V.snoc (V.drop 1 leftBorders) (x + width row)
             rightPoints = V.zipWith (\x y -> 0.3*x+0.7*y) leftBorders rightBorders
             leftPoints = V.cons x (V.take (V.length glyphs - 1) rightPoints)
             caretx = maybe caretxInit (glyphX . (glyphs V.!)) $ V.findIndex (\(px,gx) -> mx >= px && mx < gx) $ V.zip leftPoints rightPoints
         beginPath c
         fillColor c (rgba 255 192 0 255)
         rect c caretx y' 1 lineh
         fill c
         -- realized too late that I probably should have used a fold
         writeIORef gutter (Just (i+1,x-10,y'+lineh/2))
     gutter' <- readIORef gutter
     forM_ gutter' $ \(gutter,gx,gy) -> do
       let txt = T.pack $ show gutter
       fontSize c 13
       textAlign c (S.fromList [AlignRight,AlignMiddle])
       (Bounds (V4 b0 b1 b2 b3)) <- textBounds c gx gy txt
       beginPath c
       fillColor c (rgba 255 192 0 255)
       roundedRect c (b0-4) (b1-2) ((b2-b0)+8) ((b3-b1)+4) (((b3-b1)+4)/2-1)
       fill c
       fillColor c (rgba 32 32 32 255)
       NVG.text c gx gy txt

     y' <- (\x -> x+20+lineh) <$> readIORef yEnd

     fontSize c 13
     textAlign c (S.fromList [AlignLeft, AlignTop])
     textLineHeight c 1.2

     (Bounds (V4 b0 b1 b2 b3)) <- textBoxBounds c x y' 150 helpText

     let gx = abs $ (mx - (b0+b2)*0.5) / (b0 - b2)
         gy = abs $ (my - (b1+b3)*0.5) / (b1 - b3)
         a = (\x -> clamp x 0 1) $ max gx gy - 0.5
     globalAlpha c a

     beginPath c
     fillColor c (rgba 220 220 220 255)
     roundedRect c (b0-2) (b1-2) ((b2-b0)+4) ((b3-b1)+4) 3
     let px = (b2+b0)/2
     moveTo c px (b1-10)
     lineTo c (px+7) (b1+1)
     lineTo c (px-7) (b1+1)
     fill c

     fillColor c (rgba 0 0 0 220)
     textBox c x y' 150 helpText
         
     restore c
  where text = "This is longer chunk of text.\n \n Would have used lorem ipsum but she    was busy jumping over the lazy dog with the fox and all the men who came to the aid of the party."
        helpText = "Hover your mouse over the text to see calculated caret position."

drawGraph :: Context -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()
drawGraph c x y w h t =
  do bg <- linearGradient c x y x (y+h) (rgba 0 16 192 0) (rgba 0 160 192 64)
     beginPath c
     moveTo c (sx V.! 0) (sy V.! 0)
     forM_ [1..5] $ \i ->
       bezierTo c (sx V.! (i-1) + dx*0.5) (sy V.! (i-1)) (sx V.! i - dx*0.5) (sy V.! i) (sx V.! i) (sy V.! i)
     lineTo c (x+w) (y+h)
     lineTo c x (y+h)
     fillPaint c bg
     fill c

     beginPath c
     moveTo c (sx V.! 0) (sy V.! 0 + 2)
     forM_ [1..5] $ \i ->
       bezierTo c (sx V.! (i-1)+dx*0.5) (sy V.! (i-1)+2) (sx V.! i - dx*0.5) (sy V.! i + 2) (sx V.! i) (sy V.! i + 2)
     strokeColor c (rgba 0 0 0 32)
     strokeWidth c 3
     stroke c

     beginPath c
     moveTo c (sx V.! 0) (sy V.! 0)
     forM_ [1..5] $ \i ->
       bezierTo c (sx V.! (i-1)+dx*0.5) (sy V.! (i-1)) (sx V.! i - dx*0.5) (sy V.! i) (sx V.! i) (sy V.! i)
     strokeColor c (rgba 0 160 192 255)
     strokeWidth c 3
     stroke c

     V.forM_ (V.zip sx sy) $ \(x,y) ->
       do bg <- radialGradient c x (y+2) 3 8 (rgba 0 0 0 32) (rgba 0 0 0 0)
          beginPath c
          rect c (x-10) (y-10+2) 20 20
          fillPaint c bg
          fill c

     beginPath c
     V.forM_ (V.zip sx sy) $ \(x,y) -> circle c x y 4
     fillColor c (rgba 0 160 192 255)
     fill c
     beginPath c
     V.forM_ (V.zip sx sy) $ \(x,y) -> circle c x y 2
     fillColor c (rgba 220 220 220 255)
     fill c
     
     strokeWidth c 1
  where samples :: V.Vector CFloat
        samples =
          V.fromList
            [(1 + sin (t * 1.2345 + cos (t * 0.33457) * 0.44)) * 0.5
            ,(1 + sin (t * 0.68363 + cos (t * 1.3) * 1.55)) * 0.5
            ,(1 + sin (t * 1.1642 + cos (t * 0.33457) * 1.24)) * 0.5
            ,(1 + sin (t * 0.56345 + cos (t * 1.63) * 0.14)) * 0.5
            ,(1 + sin (t * 1.6245 + cos (t * 0.254) * 0.3)) * 0.5
            ,(1 + sin (t * 0.345 + cos (t * 0.03) * 0.6)) * 0.5]
        dx = w / 5
        sx :: V.Vector CFloat
        sx =
          V.generate 6
                     (\i -> x + fromIntegral i * dx)
        sy :: V.Vector CFloat
        sy = V.map (\s -> y + h * s * 0.8) samples

drawColorwheel :: Context -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()
drawColorwheel c x y w h t =
  do save c
     forM_ [0..5] $ \i ->
       do let a0 = i / 6 * pi * 2 - aeps
              a1 = (i+1)/6*pi*2+aeps
          beginPath c
          arc c cx cy r0 a0 a1 CW
          arc c cx cy r1 a1 a0 CCW
          closePath c
          let ax = cx + cos a0 * (r0+r1)*0.5
              ay = cy+sin a0 * (r0+r1)*0.5
              bx = cx + cos a1 * (r0+r1) * 0.5
              by = cy + sin a1 * (r0 + r1) * 0.5
          paint <- linearGradient c ax ay bx by (hsla (a0/(2*pi)) 1 0.55 255) (hsla (a1/(2*pi)) 1 0.55 255)
          fillPaint c paint
          fill c
     beginPath c
     circle c cx cy (r0-0.5)
     circle c cx cy (r1+0.5)
     strokeColor c (rgba 0 0 0 64)
     strokeWidth c 1
     stroke c

     save c
     translate c cx cy
     rotate c (hue*pi*2)

     strokeWidth c 2
     beginPath c
     rect c (r0-1) (-3) (r1-r0+2) 6
     strokeColor c (rgba 255 255 255 192)
     stroke c

     paint <- boxGradient c (r0-3) (-5) (r1-r0+6) 10 2 4 (rgba 0 0 0 128) (rgba 0 0 0 0)
     beginPath c
     rect c (r0-2-10) (-4-10) (r1-r0+4+20) (8+20)
     rect c (r0-2) (-4) (r1-r0+4) 8
     pathWinding c (fromIntegral$fromEnum Hole)
     fillPaint c paint
     fill c

     let r = r0 - 6
         ax = cos (120/180*pi) * r
         ay = sin(120/180*pi) * r
         bx = cos (-120/180*pi) * r
         by = sin(-120/180*pi) * r
     beginPath c
     moveTo c r 0
     lineTo c ax ay
     lineTo c bx by
     closePath c
     paint <- linearGradient c r 0 ax ay (hsla hue 1 0.5 255) (rgba 255 255 255 255)
     fillPaint c paint
     fill c
     strokeColor c (rgba 0 0 0 64)
     stroke c

     let ax = cos (120/180*pi)*r*0.3
         ay = sin(120/180*pi)*r*0.4
     strokeWidth c 2
     beginPath c
     circle c ax ay 5
     strokeColor c (rgba 255 255 255 192)
     stroke c

     paint <- radialGradient c ax ay 7 9 (rgba 0 0 0 64) (rgba 0 0 0 0)
     beginPath c
     rect c (ax-20) (ay-20) 40 40
     circle c ax ay 7
     pathWinding c (fromIntegral$fromEnum Hole)
     fillPaint c paint
     fill c
     restore c
     restore c
  where hue = sin (t * 0.12)
        cx = x + w*0.5
        cy = y+h*0.5
        r1 = min w h * 0.5 - 5
        r0 = r1 - 20
        aeps = 0.5 / r1
        

drawLines :: Context -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()
drawLines c x y w h t =
  do save c
     forM_ [0..2] $ \i -> forM_ [0..2] $ \j ->
       do let fx = x +s*0.5+(fromIntegral i*3+fromIntegral j)/9*w+pad
              fy = y-s*0.5+pad
          lineCap c (caps V.! i)
          lineJoin c (joins V.! j)

          strokeWidth c (s*0.3)
          strokeColor c (rgba 0 0 0 160)
          beginPath c
          moveTo c (fx + pts V.! 0) (fy + pts V.! 1)
          moveTo c (fx + pts V.! 2) (fy + pts V.! 3)
          moveTo c (fx + pts V.! 4) (fy + pts V.! 5)
          moveTo c (fx + pts V.! 6) (fy + pts V.! 7)
          stroke c

          lineCap c Butt
          lineJoin c Bevel

          strokeWidth c 1
          strokeColor c (rgba 0 192 255 255)
          beginPath c
          moveTo c (fx + pts V.! 0) (fy + pts V.! 1)
          moveTo c (fx + pts V.! 2) (fy + pts V.! 3)
          moveTo c (fx + pts V.! 4) (fy + pts V.! 5)
          moveTo c (fx + pts V.! 6) (fy + pts V.! 7)
          stroke c
     restore c
  where pad = 0.5
        s = w / 9 - pad * 2
        joins = V.fromList [Miter,Round,Bevel]
        caps = V.fromList [Butt,Round,Square]
        pts =
          V.fromList
            [-s * 0.25 + cos (t * 0.3) * s * 0.5
            ,sin (t * 0.3) * s * 0.5
            ,-s * 0.25
            ,0
            ,s * 0.25
            ,0
            ,s * 0.25 + cos (-t * 0.3) * s * 0.5
            ,sin (-t * 0.3) * s * 0.5]

drawWidths :: Context -> CFloat -> CFloat -> CFloat -> IO ()
drawWidths c x y width =
  do save c
     strokeColor c (rgba 0 0 0 255)
     forM_ [0..19] $ \i ->
       do let w = (i+0.5)*0.1
              y' = y + (10*i)
          strokeWidth c w
          beginPath c
          moveTo c x y'
          lineTo c (x+width) (y'+width*0.3)
          stroke c
     restore c

data DemoData =
  DemoData {fontNormal :: Font
           ,fontBold :: Font
           ,fontIcons :: Font
           ,images :: V.Vector Image}

loadDemoData :: Context -> MaybeT IO DemoData
loadDemoData c = do icons <- MaybeT $ createFont c "icons" (FileName "nanovg/example/entypo.ttf")
                    normal <- MaybeT $ createFont c "sans" (FileName "nanovg/example/Roboto-Regular.ttf")
                    bold <- MaybeT $ createFont c "sans-bold" (FileName "nanovg/example/Roboto-Bold.ttf")
                    images <- loadImages
                    pure (DemoData icons normal bold images)
  where loadImages :: MaybeT IO (V.Vector Image)
        loadImages =
          V.generateM 12 $
          \i ->
            do let file = FileName $
                     "nanovg/example/images/image" <> T.pack (show (i + 1)) <> ".jpg"
               MaybeT $ createImage c file 0

drawCaps :: Context -> CFloat -> CFloat -> CFloat -> IO ()
drawCaps c x y width =
  do save c

     beginPath c
     rect c (x-lineWidth/2) y (width+lineWidth) 40
     fillColor c (rgba 255 255 255 32)
     fill c

     beginPath c
     rect c x y width 40
     fillColor c (rgba 255 255 255 32)
     fill c

     strokeWidth c lineWidth
     forM_ (zip [0..] [Butt,Round,Square]) $ \(i,cap) ->
       do lineCap c cap
          strokeColor c (rgba 0 0 0 255)
          beginPath c
          moveTo c x (y+i*10+5)
          lineTo c (x+width) (y+i*10+5)
          stroke c
     restore c
  where lineWidth = 8

drawScissor :: Context -> CFloat -> CFloat -> CFloat -> IO ()
drawScissor c x y t =
  do save c

     translate c x y
     rotate c (degToRad 5)
     beginPath c
     rect c (-20) (-20) 60 40
     fillColor c (rgba 255 0 0 255)
     fill c
     scissor c (-20) (-20) 60 40

     translate c 40 0
     rotate c t

     save c
     resetScissor c
     beginPath c
     rect c (-20) (-10) 60 30
     fillColor c (rgba 255 128 0 64)
     fill c
     restore c

     intersectScissor c (-20) (-10) 60 30
     beginPath c
     rect c (-20) (-10) 60 30
     fillColor c (rgba 255 128 0 255)
     fill c

     restore c

drawComposite :: Context -> IO ()
drawComposite c = do
  -- The example in https://github.com/memononen/nanovg/pull/298 implies the
  -- need to call beginFrame/endFrame, but it does not seem to be necessary
  save c

  -- Clears a fragment of the background
  globalCompositeOperation c DestinationOut
  beginPath c
  rect c 50 50 100 100
  fillColor c (rgb 0 0 0)
  fill c

  -- Draws a red circle
  globalCompositeOperation c SourceOver
  beginPath c
  circle c 100 100 30
  fillColor c (rgb 255 0 0)
  fill c

  -- Draws a blue, rounded, rectangle behind the red circle
  globalCompositeOperation c DestinationOver
  beginPath c
  roundedRectVarying c 60 60 80 60 20 10 20 10
  fillColor c (rgb 0 0 255)
  fill c

  restore c

clamp :: Ord a => a -> a -> a -> a
clamp a' low up
  | a' < low = low
  | a' > up = up
  | otherwise = a'
