{-# LANGUAGE OverloadedStrings #-}
module Main where

import SDL
import SDL.Video.Renderer
import SDL.Vect
import Linear (V4(..))
import Control.Monad (unless)

main :: IO ()
main = do
  initializeAll
  window <- createWindow "Martin der Roboter" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer

appLoop :: Renderer -> IO ()
appLoop renderer = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  rendererDrawColor renderer $= V4 255 255 255 255
  clear renderer
  rendererDrawColor renderer $= V4 0 0 0 255
  -- drawLine renderer (P (V2 1 1)) (P (V2 800 600))
  fillRect renderer (Just $ Rectangle (P (V2 50 50)) (V2 100 50))
  present renderer
  unless qPressed (appLoop renderer)

