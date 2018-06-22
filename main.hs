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

main :: IO () 
main = start $ do
  f <- frame [text:="Martin der Roboter"] 
  p <- panel f [ ]
  set f [ layout := minsize (sz (round Geometry.width) (round Geometry.height)) $ widget p ]
  frameCenter f

  t <- timer f [ interval := 10 ]      -- update
  t2 <- timer f [ interval := 300 ]   -- shooting
  t3 <- timer f [ interval := 5000 ]    -- raindrops
  
  
  SDL.init [SDL.InitAudio]
  result <- openAudio 22050 Mix.AudioS16LSB 2 4096
  music <- Mix.loadWAV "spielsong2.wav"
  pew <- Mix.loadWAV "pew2.wav"
  explosion <- Mix.loadWAV "shot2.wav"
  bgMusic <- Mix.playChannel (-1) music (-1)
  shotSound <- Mix.playChannel (-1) pew (-1)
  Mix.pause shotSound
  
  let networkDescription :: MomentIO ()
      networkDescription = mdo
        
        (bGameRunning :: Behavior (Martin))
            <- stepper (initialMartin) $
                 updateMartin <$> (filterJust $ justMove <$> emouse)
      
        etick <- event0 t command   -- timer for updates
        etick2 <- event0 t2 command -- timer for shooting
        etick3 <- event0 t3 command -- timer for creating new raindrops
        
        reactimate $ (touchForeignPtr music >> touchForeignPtr pew >> touchForeignPtr explosion) <$ etick2
        
        ekey <- event1 p keyboard   -- keyboard events
        emouse <- event1 p mouse    -- mouse events

        let nTyped  = filterE ((== KeyChar 'N') . keyKey) ekey
            onNewGame = filterE ((== KeyRight) . keyKey) ekey
            eup    = filterE ((== KeyUp   ) . keyKey) ekey
            edown  = filterE ((== KeyDown ) . keyKey) ekey
        
        (bMartin :: Behavior (Martin))
            <- stepper (initialMartin) $
                 updateMartin <$> (filterJust $ justMove <$> emouse)
        
        
        brandom <- fromPoll (randomRIO (0,1) :: IO Float)
        
        reactimate $ (\ shooting -> 
                          if shooting then
                            resume shotSound 
                          else 
                            Mix.pause shotSound) <$>  (bShooting <@ emouse)
        
        (bShots :: Behavior Shots)
            <- accumB [] $ unions
                 [ addShot <$> (bMartin) <@ whenE bShooting etick2
                 , (updateShots <$> bDrops) <@ etick
                 , (\ _ -> [] ) <$ onNewGame
                 ]
        
        (bDrops :: Behavior Drops)
            <- accumB initialDrops $ unions
                 [ addDrop <$> brandom <@ etick3
                 , (updateDrops <$> bShots) <@ etick
                 , (\ _ -> []) <$ onNewGame
                 ]
        
        (bShooting :: Behavior Bool)
            <- stepper False $ (filterJust $ justPressed <$> emouse)
        
        (bBackground :: Behavior (Int,Int))
            <- accumB (0,0) $ updateBackground <$ etick
        
        (bScore :: Behavior Int)
            <- accumB 0 $ unions
                 [ (+1)       <$ etick
                 , (\ _ -> 0) <$ onNewGame
                 ]
        
        bpaint <- stepper (\_dc _ -> return ()) $ (render <$> bMartin <*> bShots <*> bDrops <*> bBackground <*> bScore) <@ etick
        
        sink p [on paint :== bpaint]
        reactimate $ repaint p <$ etick
        
        reactimate $ (set t  [enabled := False] >>
                      set t2 [enabled := False] >>
                      set t3 [enabled := False] >>
                      Mix.pause shotSound >> 
                      Mix.pause bgMusic )
                   <$ whenE (intersectsMartin <$> bDrops <*> bMartin) etick
        
        reactimate $ (set t  [enabled := True] >>
                      set t2 [enabled := True] >>
                      set t3 [enabled := True] >> 
                      resume bgMusic)
                   <$ onNewGame

        reactimate $ (Mix.playChannel (-1) explosion 0 >> return ()) <$ whenE (intersectionShotDrop <$> bShots <*> bDrops) etick
        
        return ()
  
  forkIO $ do 
      network <- compile networkDescription
      actuate network

  return ()

justPressed :: EventMouse -> Maybe Bool
justPressed (MouseLeftDown _ _) = Just True
justPressed (MouseLeftUp _ _) = Just False
justPressed _ = Nothing

justMove :: EventMouse -> Maybe Point
justMove (MouseMotion pt _) = Just pt
justMove (MouseLeftDrag pt _) = Just pt
justMove (MouseRightDrag pt _) = Just pt
justMove _                  = Nothing
