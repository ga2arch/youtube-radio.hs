{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent
import Control.Monad

import Radio
import Streamer

main :: IO ()
main = do
  (pid, env) <- runStreamer 8000

  let radio1 = RadioConfig bombz "/onlybombz" 320
  let radio2 = RadioConfig asmr "/asmr" 320
  let radio3 = RadioConfig rap "/rap" 320
  runRadios env [radio1, radio2, radio3]

  wait

  where
    wait = forever $ threadDelay $ 1000 * 1000 * 10
    bombz = randomPlaylist "playlists/bombz"
    asmr = randomPlaylist "playlists/asmr"
    rap = randomPlaylist "playlists/rap"
