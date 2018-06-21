module Sound
(
  play
) where
import Control.Monad
import Control.Monad.Fix
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Mixer as Mix

play :: String -> IO ()
play filename = do
  SDL.init [SDL.InitAudio]
  result <- openAudio audioRate audioFormat audioChannels audioBuffers
  pew <- Mix.loadWAV filename
  ch1 <- Mix.playChannel anyChannel pew 0
  fix $ \loop -> do
    SDL.delay 50
    stillPlaying <- numChannelsPlaying
    when (stillPlaying /= 0) loop
  Mix.closeAudio
  SDL.quit

  where audioRate     = 22050
        audioFormat   = Mix.AudioS16LSB
        audioChannels = 2
        audioBuffers  = 4096
        anyChannel    = (-1)
