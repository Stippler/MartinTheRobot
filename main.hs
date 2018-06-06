{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

import Graphics.UI.WX hiding (Event)
import Reactive.Banana 
import Reactive.Banana.WX hiding (compile)
import Object
import Geometry
import Data.Function hiding (on)
import Control.Lens hiding (set)

width = 800
height = 600

main :: IO ()
main = start $ do 
  f <- frame [text:="Martin der Roboter"]
  p <- panel f [ ]
  -- set f [ layout := minsize (sz width height) $ widget p ]
  t <- timer f [ interval := 10 ]
  t2 <- timer f [ interval := 100 ]
  
  let networkDescription :: MomentIO ()
      networkDescription = mdo
      
        etick <- event0 t command   -- timer for updates
        etick2 <- event0 t2 command -- timer for shooting
        ekey <- event1 p keyboard   -- keyboard events
        emouse <- event1 p mouse    -- mouse events

        let eleft  = filterE ((== KeyLeft ) . keyKey) ekey
            eright = filterE ((== KeyRight) . keyKey) ekey
            eup    = filterE ((== KeyUp   ) . keyKey) ekey
            edown  = filterE ((== KeyDown ) . keyKey) ekey
                
        (bPlayerPosition :: Behavior (Martin))
            <- stepper (Circle 0 0 0) $
                 toCircle 15 <$> (filterJust $ justMove <$> emouse)
        
        (bShotsDrops :: Behavior (Shots,Drops))
            <- accumB ([], [CircleVec (Circle 10 20 10) (Vec 0 0), CircleVec (Circle 60 00 10) (Vec 0 0), CircleVec (Circle 110 (-20) 10) (Vec 0 0), CircleVec (Circle 160 (-40) 10) (Vec 0 0)]) $ unions
                 [ addShot <$> (((\ a b -> if a then (Just b) else Nothing) <$> bShooting <*> (bPlayerPosition) <@ etick2 ))
                 , (\(shots, drops) -> reboundShotDropPair (map move shots, map (moveAcc 0.005) drops) ) <$ etick  
                 ]
        
        
        
        (bShooting :: Behavior Bool)
            <- stepper False $ (filterJust $ justPressed <$> emouse)
              

        
        bpaint <- stepper (\_dc _ -> return ()) $ (render <$> bPlayerPosition <*> bShotsDrops ) <@ etick
      
        sink  p [on paint :== bpaint]
        reactimate $ repaint p <$ etick
        
        return ()
  
  network <- compile networkDescription
  actuate network

  return ()

toCircle :: Float -> Point2 Int  -> Circle
toCircle radius point = Circle (fromIntegral (pointX point)) (fromIntegral (pointY point)) radius


render :: Circle -> ([CircleVec], [CircleVec]) -> DC a -> Rect -> IO ()
render circle (shots, circles) dc viewArea = do
  set dc [brushColor := blue, brushKind := BrushSolid]
  renderCircle dc circle
  set dc [brushColor := red, brushKind := BrushSolid]
  mapM ((renderCircle dc) . _circle) circles
  set dc [brushColor := green, brushKind := BrushSolid]
  mapM ((renderCircle dc) . _circle) shots 
  return ()

renderCircle :: DC a -> Circle -> IO ()
renderCircle dc c = do
  Graphics.UI.WX.circle dc (point (round $ c^.x) (round $ c^.y)) (round $ c^.r) []


addShot :: Maybe Circle -> ([CircleVec], [CircleVec]) -> ([CircleVec], [CircleVec])
addShot Nothing t = t
addShot (Just circle) (shots, drops) = ((CircleVec circle $ Vec 0 (-2)):shots, drops)

justPressed :: EventMouse -> Maybe Bool
justPressed (MouseLeftDown _ _) = Just True
justPressed (MouseLeftUp _ _) = Just False
justPressed _ = Nothing

justMove :: EventMouse -> Maybe Point
justMove (MouseMotion pt _) = Just pt
justMove (MouseLeftDrag pt _) = Just pt
justMove (MouseRightDrag pt _) = Just pt
justMove _                  = Nothing

