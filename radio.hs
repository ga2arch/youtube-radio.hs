{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Data.Conduit.Process.Unix (forkExecuteFile, waitForProcess)
import System.Process (readProcess)
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
