{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

import Graphics.UI.WX hiding (Event)
import Reactive.Banana 
import Reactive.Banana.WX hiding (compile)
import Object
import Data.Function hiding (on)
import Control.Lens hiding (set)
import Graphics.UI.WXCore
import System.Random
import Geometry
import Sound
import Control.Concurrent
import Control.Monad
import Control.Monad.Fix
import Graphics.UI.SDL as SDL hiding (Event, KeyUp, KeyDown, MouseMotion)
import Graphics.UI.SDL.Mixer as Mix
import Foreign.ForeignPtr

-- http://hackage.haskell.org/package/wx-0.92.3.0/docs/Graphics-UI-WX-Attributes.html#v::-126-

main :: IO () 
main = start $ do
  -- setup --
  f <- frame [text:="Martin der Roboter"] 
  p <- panel f [ ]
  set f [ layout := minsize (sz (round Geometry.width) (round Geometry.height)) $ widget p ]
  frameCenter f

  -- timers --
  t  <- timer f [ interval := 10 ]      -- update, rendering
  t2 <- timer f [ interval := 300 ]     -- shooting, creation of new shots
  t3 <- timer f [ interval := 5000 ]    -- creation of new raindrops
  
  -- audio --   https://wiki.libsdl.org/SDL_OpenAudio
  SDL.init [SDL.InitAudio] 
                   -- frequency format          channels samples
  result <- openAudio 22050     Mix.AudioS16LSB 2        4096  
  music <- Mix.loadWAV "spielsong2.wav"
  pew <- Mix.loadWAV "pew2.wav"
  explosion <- Mix.loadWAV "shot2.wav" 
                          -- channel music loops
  bgMusic <- Mix.playChannel (-1)    music (-1) 
  shotSound <- Mix.playChannel (-1) pew (-1)
  Mix.pause shotSound
  
  let networkDescription :: MomentIO ()
      networkDescription = mdo
        
      ----- EVENTS -----   
        etick  <- event0 t  command
        etick2 <- event0 t2 command
        etick3 <- event0 t3 command
         
        ekey <- event1 p keyboard   -- keyboard events 
        emouse <- event1 p mouse    -- mouse events

        let qTyped    = filterE ((== KeyChar 'q') . keyKey) ekey
            onNewGame = filterE ((== KeySpace) . keyKey) ekey
            eup       = filterE ((== KeyUp   ) . keyKey) ekey
            edown     = filterE ((== KeyDown ) . keyKey) ekey


        brandomPos  <- fromPoll (randomRIO (0,1) :: IO Float)
        brandomSize <- fromPoll (randomRIO (0,1) :: IO Float)
              
        -- protects sounds from grabage collector
        reactimate $ (touchForeignPtr music >> touchForeignPtr pew >> touchForeignPtr explosion) <$ etick2
        
        ----- BEHAVIORS -----
        -- player
        (bGameRunning :: Behavior (Martin))
            <- stepper (initialMartin) $
                 updateMartin <$> (filterJust $ justMove <$> emouse)
        
        (bMartin :: Behavior (Martin))
            <- stepper (initialMartin) $
                 updateMartin <$> (filterJust $ justMove <$> emouse)
                
        -- behavior of mouse click
        (bShooting :: Behavior Bool)
            <- stepper False $ (filterJust $ justPressed <$> emouse)
            
        -- list of shots; adds shots, when bShooting; moves shots; reset on onNewGame
        (bShots :: Behavior Shots)
            <- accumB [] $ unions
                 [ addShot <$> (bMartin) <@ whenE bShooting etick2
                 , (updateShots <$> bDrops) <@ etick
                 , (\ _ -> [] ) <$ onNewGame 
                 ]
        
        -- shot-sound
        reactimate $ (\ shooting -> 
                          if shooting then
                            resume shotSound 
                          else 
                            Mix.pause shotSound) <$>  (bShooting <@ emouse)
        
        -- list of drops; adds up to 16 drops; moves drops; resets on onNewGame 
        (bDrops :: Behavior Drops)
            <- accumB initialDrops $ unions
                 [ addDrop <$> brandomPos <*> brandomSize <@ whenE (fmap (<16) $ length <$> bDrops)  etick3
                 , (updateDrops <$> bShots) <@ etick
                 , (\ _ -> []) <$ onNewGame
                 ]
        
        -- score, counts duration of game in 10^(-2) seconds; resets to 0 on onNewGame
        (bScore :: Behavior Int)
            <- accumB 0 $ unions
                 [ (+1)       <$ etick
                 , (const 0) <$ onNewGame  
                 ]
                 
        -- moves background         
        (bBackground :: Behavior (Int,Int))
            <- accumB (0,0) $ updateBackground <$ etick
        
        -- collects all renderable behaviors in bpaint :: Behavior (IO ())
        bpaint <- stepper (\_dc _ -> return ()) $ (render <$> bMartin <*> bShots <*> bDrops <*> bBackground <*> bScore) <@ etick
         
        -- paint double buffered to current view rectangle 
        sink p [on paint :== bpaint]
        -- emit a paint event to panel p
        reactimate $ repaint p <$ etick
        
        -- disable timer and sounds when game over
        reactimate $ (set t  [enabled := False] >>
                      set t2 [enabled := False] >>
                      set t3 [enabled := False] >>
                      Mix.pause shotSound >> 
                      Mix.pause bgMusic )
                   <$ whenE (intersectsMartin <$> bDrops <*> bMartin) etick
        
        -- enable timer and background music on onNewGame
        reactimate $ (set t  [enabled := True] >>
                      set t2 [enabled := True] >>
                      set t3 [enabled := True] >> 
                      resume bgMusic)
                   <$ onNewGame

        -- explosive sounds
        reactimate $ (Mix.playChannel (-1) explosion 0 >> return ()) <$ whenE (intersectionShotDrop <$> bShots <*> bDrops) etick
        
        return ()
  
  forkIO $ do 
      network <- compile networkDescription
      actuate network

  return ()

-- | get Maybe Bool from 
justPressed :: EventMouse -> Maybe Bool
justPressed (MouseLeftDown _ _) = Just True
justPressed (MouseLeftUp _ _) = Just False
justPressed _ = Nothing

-- | get (x, y)-coordinates from mouse position
justMove :: EventMouse -> Maybe Point
justMove (MouseMotion pt _) = Just pt
justMove (MouseLeftDrag pt _) = Just pt
justMove (MouseRightDrag pt _) = Just pt
justMove _                  = Nothing
