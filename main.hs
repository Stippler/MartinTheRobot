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

main :: IO ()
main = start $ do 
  playMusic
  f <- frame [text:="Martin der Roboter"]
  --set f [layout := minsize (sz 800 600)]
  p <- panel f [ ]
  set f [ layout := minsize (sz width height) $ widget p ]
  frameCenter f

  t <- timer f [ interval := 10 ]
  t2 <- timer f [ interval := 300 ]
  t3 <- timer f [ interval := 300 ]
    
  
  
  let networkDescription :: MomentIO ()
      networkDescription = mdo
      
        etick <- event0 t command   -- timer for updates
        etick2 <- event0 t2 command -- timer for shooting
        etick3 <- event0 t3 command -- timer for raindrops
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
                   addDrop <$> brandom <@ etick2
                 , addShot <$> (((\ a b -> if a then (Just b) else Nothing) <$> bShooting <*> (bPlayerPosition) <@ etick2 ))
                 , updateDropShotPair <$ etick  
                 ]
        
        (bShooting :: Behavior Bool)
            <- stepper False $ (filterJust $ justPressed <$> emouse)
              
        (bBackground :: Behavior (Int,Int))
            <- accumB (0,0) $ updateBackground <$ etick
        
        bpaint <- stepper (\_dc _ -> return ()) $ (render <$> bPlayerPosition <*> bShotsDrops <*> bBackground <*> bShooting) <@ etick
        
        sink  p [on paint :== bpaint]
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

