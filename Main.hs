module Main where

import Graphics.Pixelflut

main :: IO ()
main = withConnection "nyx" "1234" $ do
  px 100 100 red
  px 100 102 green
  px 100 104 blue
  px 100 106 cyan
  px 100 108 yellow
  px 100 110 magenta

