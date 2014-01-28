{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.STM
import Data.Conduit
import Data.Conduit.Process.Unix (forkExecuteFile, waitForProcess)
import Data.Conduit.TMChan
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import System.IO (readFile)
import System.Process (readProcess)
import System.Random (randomRIO)

import qualified Blaze.ByteString.Builder.ByteString as BBB
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.ByteString.UTF8 as BU

--------------------------------------

mpv url = do
  p <- forkExecuteFile
         "mpv"
         ["--no-video",
          "-ao",
          "pcm:file=/tmp/pipe.mp3",
          "--really-quiet",
         (BU.fromString url)]
         Nothing
         Nothing
         Nothing
         Nothing
         Nothing
  return p

lame = do
  p <- forkExecuteFile
          "lame"
          ["--quiet",
           "/tmp/pipe.mp3",
           "/tmp/radio.mp3"]
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
  return p

--------------------------------------

streamUrl yurl = do
  tmp <- readProcess "youtube-dl" ["-g", yurl] []
  let url = take (length tmp - 1) tmp

  when (url !! 4 /= 's') $ do
    mpv url
    l <- lame
    _ <- waitForProcess l
    return()

queue = do
  yurls <- fmap lines (readFile "playlist")
  yurl <- pick yurls
  streamUrl yurl
  queue
  where
    pick xs = randomRIO (0, (length xs - 1)) >>= return . (xs !!)

radio c = do
  runResourceT $
    CB.sourceFile "/tmp/radio.mp3"
    $= CL.map (Chunk . BBB.fromByteString)
    $$ sinkTMChan c False

app input _ = do
  chan <- atomically $ dupTMChan input
  return $ responseSource status200 [] $ sourceTMChan chan
