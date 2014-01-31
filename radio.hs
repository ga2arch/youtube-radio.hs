{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, KindSignatures #-}
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

youtubeDl yurl = do
  out <- atomically $ newTBMChan 16
  y <- forkExecuteFile "youtube-dl"
       ["-g", (BU.fromString yurl)]
       Nothing Nothing
       (Just $ return ())
       (Just $ sinkTBMChan out True)
       (Just $ CL.sinkNull)
  url <- runResourceT $ sourceTBMChan out $$ CB.sinkLbs
  return url

ffmpeg out url = do
  f <- forkExecuteFile "ffmpeg"
       ["-i", (toStrict . BC.init $ url), "-vn", "-f", "mp3", "-"]
       Nothing Nothing
       (Just $ return ())
       (Just $ sinkTBMChan out False)
       (Just $ CL.sinkNull)
  _ <- waitForProcess f
  return ()
  where
    toStrict = head . BC.toChunks

sinkMpv :: ConduitM BU.ByteString Void (ResourceT IO) ()
sinkMpv = do
  input <- liftIO . atomically $ newTBMChan 1024
  m <- liftIO $ forkExecuteFile "mpv"
       ["-volume", "0", "-"]
       Nothing Nothing
       (Just $ sourceTBMChan input)
       (Just $ CL.sinkNull)
       (Just $ CL.sinkNull)

  bracketP (return input)
           (atomically . closeTBMChan)
           ((flip sinkTBMChan) False)

bombz out = do
  print "START QUEUE"
  yurls <- fmap lines $ readFile "bombz"
  yurl <- pick yurls

  url <- youtubeDl yurl
  print url
  unless (BC.isPrefixOf "https" url) $ ffmpeg out url
  where
    pick ls = fmap (ls !!) $ randomRIO (0, (length ls - 1))

asmr out = do
  print "START QUEUE"
  yurls <- fmap lines $ readFile "asmr"
  yurl <- pick yurls

  url <- youtubeDl yurl
  print url
  unless (BC.isPrefixOf "https" url) $ ffmpeg out url
  where
    pick ls = fmap (ls !!) $ randomRIO (0, (length ls - 1))

sourceRadio handle = do
  out <- liftIO . atomically $ newTBMChan 1024
  liftIO . forkIO . forever $
    catch
      (handle out)
      (\(e ::SomeException) -> handle out)

  bracketP (return out)
           (\_ -> atomically $ closeTBMChan out)
           (sourceTBMChan)

main = do
  (env, app) <- initServer
  forkIO $ runResourceT $ sourceRadio bombz $= conduitStreamer env "/onlybombz" $$ sinkMpv
  forkIO $ runResourceT $ sourceRadio asmr $= conduitStreamer env "/asmr" $$ sinkMpv
  runServer app 8000
