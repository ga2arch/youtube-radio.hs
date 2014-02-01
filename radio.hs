{-# LANGUAGE OverloadedStrings,
             ScopedTypeVariables,
             KindSignatures,
             RecordWildCards #-}

module Main where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Data.Conduit
import           Data.Conduit.Process.Unix
import           Data.Conduit.TMChan
import           Data.Void
import           System.Random (randomRIO)
import           Streamer

import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL

--------------------------------------

data RadioConfig = RadioConfig
                   { radioHandle :: IO (Maybe BC.ByteString)
                   , radioMount  :: BU.ByteString
                   , radioBitrate :: Int }
--------------------------------------

youtubeDl yurl = do
  out <- atomically $ newTBMChan 16
  y <- forkExecuteFile "youtube-dl"
       ["-g", (BU.fromString yurl)]
       Nothing Nothing
       (Just $ return ())
       (Just $ sinkTBMChan out True)
       (Just $ CL.sinkNull)
  _ <- waitForProcess y
  url <- runResourceT $ sourceTBMChan out $$ CB.sinkLbs
  return url

ffmpeg out bitrate url = do
  f <- forkExecuteFile "ffmpeg"
       ["-i", (toStrict . BC.init $ url),
        "-vn", "-f", "mp3",
        "-ab", (BU.fromString $ (show bitrate) ++ "k"),
        "-"]
       Nothing Nothing
       (Just $ return ())
       (Just $ sinkTBMChan out False)
       (Just $ CL.sinkNull)
  _ <- waitForProcess f
  return ()
  where
    toStrict = head . BC.toChunks

randomPlaylist pls = do
  print "START QUEUE"
  yurls <- fmap lines $ readFile pls
  yurl <- pick yurls

  url <- youtubeDl yurl
  print url
  if (BC.isPrefixOf "https" url)
    then return Nothing
    else return $ Just url
  where
    pick ls = fmap (ls !!) $ randomRIO (0, (length ls - 1))

sourceRadio bitrate handle = do
  out <- liftIO . atomically $ newTBMChan 16
  liftIO . forkIO . forever $ catch (do
      res <- handle
      case res of
          Just url -> ffmpeg out bitrate url)
    (\(_ :: SomeException) -> return())

  bracketP (return out)
           (\_ -> atomically $ closeTBMChan out)
           (sourceTBMChan)

runRadios env = mapM_ (forkIO . runRadio env)

runRadio env (RadioConfig{..}) =
  runResourceT $
    sourceRadio radioBitrate radioHandle
    $= conduitStreamer env radioMount
    $$ sinkFakeListener radioBitrate

sinkFakeListener bitrate =
  (CL.sequence $
   (CB.drop (truncate $ 1024 * (fromIntegral bitrate) * 0.125)
    >> (liftIO $ threadDelay $ 1000*1000)))
  =$ CL.sinkNull

main = do
  (pid, env) <- runStreamer 8000

  let radio1 = RadioConfig bombz "/onlybombz" 320
  let radio2 = RadioConfig asmr "/asmr" 320
  let radio3 = RadioConfig rap "/rap" 320

  runRadios env [radio1, radio2, radio3]
  wait

  where
    wait = forever $ threadDelay $ 1000 * 1000 * 10
    bombz = randomPlaylist "bombz"
    asmr = randomPlaylist "asmr"
    rap = randomPlaylist "rap"
