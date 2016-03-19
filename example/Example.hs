{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           Control.Monad.Loops
import           Data.Bits hiding (rotate)
import qualified Data.Set as S
import qualified Data.Vector as V
import           Graphics.GL.Core32
import           Graphics.UI.GLFW
import           NanoVG
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
                 renderDemo c mx my width height t
                 endFrame c
                 swapBuffers w
                 pollEvents

renderDemo :: Context -> Double -> Double -> Int -> Int -> Double -> IO ()
renderDemo c mx my w h t =
  do drawEyes c (fromIntegral w - 250) 50 150 100 (realToFrac mx) (realToFrac my) (realToFrac t)
     drawParagraph c (fromIntegral w - 450) 50 150 100 (realToFrac mx) (realToFrac my)
     drawGraph c 0 (fromIntegral h/2) (fromIntegral w) (fromIntegral h/2) (realToFrac t)
     drawColorwheel c (fromIntegral w - 300) (fromIntegral h - 300) 250 250 (realToFrac t)

     drawLines c 120 (fromIntegral h - 50) 600 50 (realToFrac t)

     drawWidths c 10 50 30

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
drawParagraph c x y width height mx my =
  do save c
     fontSize c 18
     fontFace c "sans"
     textAlign c (S.fromList [AlignLeft,AlignTop])
     (_,_,lineh) <- textMetrics c
     pure ()
  where text = "This is longer chunk of text.\n \n Would have used lorem ipsum but she    was busy jumping over the lazy dog with the fox and all the men who came to the aid of the party."

drawGraph :: Context -> CFloat -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()
drawGraph c x y w h t =
  do bg <- linearGradient c x y x (y+h) (rgba 0 16 192 0) (rgba 0 160 192 64)
     beginPath c
     moveTo c (sx V.! 0) (sy V.! 0)
     forM_ [1..5] $ \i -> bezierTo c (sx V.! (i-1) + dx*0.5) (sy V.! (i-1)) (sx V.! i - dx*0.5) (sy V.! i) (sx V.! i) (sy V.! i)
     lineTo c (x+w) (y+h)
     lineTo c x (y+h)
     fillPaint c bg
     fill c

     beginPath c
     moveTo c (sx V.! 0) (sy V.! 0 + 2)
     forM_ [1..5] $ \i -> bezierTo c (sx V.! (i-1)+dx*0.5) (sy V.! (i-1)+2) (sx V.! i - dx*0.5) (sy V.! i + 2) (sx V.! i) (sy V.! i + 2)
     strokeColor c (rgba 0 0 0 32)
     strokeWidth c 3
     stroke c

     beginPath c
     moveTo c (sx V.! 0) (sy V.! 0)
     forM_ [1..5] $ \i -> bezierTo c (sx V.! (i-1)+dx*0.5) (sy V.! (i-1)) (sx V.! i - dx*0.5) (sy V.! i) (sx V.! i) (sy V.! i)
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
     let c0 = rgba 0 0 0 255
     forM_ [0..19] $ \i ->
       do let w = (i+0.5)*0.1
              y' = y + (10*i)
          strokeWidth c w
          beginPath c
          moveTo c x y'
          lineTo c (x+width) (y'+width*0.3)
          stroke c
     restore c
