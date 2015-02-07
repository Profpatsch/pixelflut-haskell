module Main where


import Graphics.Pixelflut

run = withConnection "172.16.0.163" "1234"

main :: IO ()
main = run $ do
  px 100 100 red
  px 100 102 green
  px 100 104 blue
  px 100 106 cyan
  px 100 108 yellow
  px 100 110 magenta

