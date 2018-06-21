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
import Control.Parallel
import Control.Concurrent

main :: IO ()
main = start $ do
  forkIO (Sound.play "spielsong2.wav")
  f <- frame [text:="Martin der Roboter"] 
  p <- panel f [ ]
  set f [ layout := minsize (sz (round Geometry.width) (round Geometry.height)) $ widget p ]
  frameCenter f

  t <- timer f [ interval := 10 ]      -- update
  t2 <- timer f [ interval := 300 ]   -- shooting
  t3 <- timer f [ interval := 5000 ]    -- raindrops
  
  let networkDescription :: MomentIO ()
      networkDescription = mdo
        
        (bGameRunning :: Behavior (Martin))
            <- stepper (initialMartin) $
                 updateMartin <$> (filterJust $ justMove <$> emouse)
      
        etick <- event0 t command   -- timer for updates
        etick2 <- event0 t2 command -- timer for shooting
        etick3 <- event0 t3 command -- timer for creating new raindrops

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
        
        bpaint <- stepper (\_dc _ -> return ()) $ (render <$> bMartin <*> bShots <*> bDrops <*> bBackground <*> bShooting) <@ etick
        
        sink p [on paint :== bpaint]
        reactimate $ repaint p <$ etick
        
        reactimate $ (set t [enabled :~ not] >>
                      set t2 [enabled :~ not] >>
                      set t3 [enabled :~ not])
                   <$ whenE (intersectsMartin <$> bDrops <*> bMartin) etick
        
        reactimate $ (set t [enabled :~ not] >>
                      set t2 [enabled :~ not] >>
                      set t3 [enabled :~ not])
                   <$ onNewGame

        reactimate $  (Sound.play "pew.wav") <$ whenE bShooting etick2
        
        return ()
  
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
