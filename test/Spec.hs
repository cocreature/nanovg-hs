module Main where

import Test.Hspec
import qualified NanoVGSpec as NanoVG

main :: IO ()
main = hspec NanoVG.spec
