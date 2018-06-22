module Sound
(
  play
) where
import Control.Monad
import Control.Monad.Fix
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Mixer as Mix
import Foreign.ForeignPtr

play :: String -> IO ()
play filename = do
  SDL.init [SDL.InitAudio]
  result <- openAudio 22050 Mix.AudioS16LSB 2 4096
  music <- Mix.loadWAV filename
  ch1 <- Mix.playChannel (-1) music 0
  return ()


{-
  fix $ \loop -> do
    touchForeignPtr music
    SDL.delay 50
    stillPlaying <- numChannelsPlaying
    when (stillPlaying /= 0) loop
  Mix.closeAudio
  SDL.quit
-}
