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

width = 800
height = 600

---------

main :: IO ()
main = do
  start martinGame

---------

martinGame :: IO ()
martinGame = do 
  --playMusic
  f <- frame [text:="Martin der Roboter"] 
  frameCenter f

  t <- timer f [ interval := 10 ]      -- update
  t2 <- timer f [ interval := 300 ]   -- shooting
  t3 <- timer f [ interval := 5000 ]    -- raindrops
  
  game  <- menuPane      [ text := "&   Pause: Ctrl+Space & -  Restart: Ctrl+N & -  Quit: Ctrl+Q" ]
  new   <- menuItem game [ text := "&New\tCtrl+N", help := "New game" ]
  pause <- menuItem game [ text      := "&Pause\tCtrl+Space" 
                         --, checkable := True
                         ] 

  menuLine game
  quit  <- menuQuit game [help := "Quit the game"]

  set new   [on command := do {close f; martinGame; propagateEvent}]
  set pause [on command := set t [enabled :~ not] ] 
  set quit  [on command := close f]

  set f [menuBar := [game]]
  
  p <- panel f [ ]
  set f [ layout := minsize (sz width height) $ widget p ]
  
  -----------------
  let networkDescription :: MomentIO ()
      networkDescription = mdo
      
        etick <- event0 t command   -- timer for updates
        etick2 <- event0 t2 command -- timer for shooting
        etick3 <- event0 t3 command -- timer for creating new raindrops

        ekey <- event1 p keyboard   -- keyboard events
        emouse <- event1 p mouse    -- mouse events

        let eleft  = filterE ((== KeyLeft ) . keyKey) ekey
            eright = filterE ((== KeyRight) . keyKey) ekey
            eup    = filterE ((== KeyUp   ) . keyKey) ekey
            edown  = filterE ((== KeyDown ) . keyKey) ekey
        
        (bPlayerPosition :: Behavior (Martin))
            <- stepper (initialMartin) $
                 updateMartin <$> (filterJust $ justMove <$> emouse)
        
        brandom <- fromPoll (randomRIO (0,1) :: IO Float)

        (bShotsDrops :: Behavior (Shots,Drops))
            <- accumB ([], initialDrops) $ unions
                 [ 
                   addDrop <$> brandom <@ etick3
                 , addShot <$> (bPlayerPosition) <@ whenE bShooting etick2 
                 , updateDropShotPair <$ etick 
                 ]
        
        reactimate $ set t [enabled :~ not] <$ whenE (intersectsMartin <$> bShotsDrops <*> bPlayerPosition) etick
        
        
        
        (bShooting :: Behavior Bool)
            <- stepper False $ (filterJust $ justPressed <$> emouse)
              
        (bBackground :: Behavior (Int,Int))
            <- accumB (0,0) $ updateBackground <$ etick
        
        bpaint <- stepper (\_dc _ -> return ()) $ (render <$> bPlayerPosition <*> bShotsDrops <*> bBackground <*> bShooting) <@ etick
        
        sink p [on paint :== bpaint]
        reactimate $ repaint p <$ etick
        
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