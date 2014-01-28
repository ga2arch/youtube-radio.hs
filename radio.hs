{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Conduit.Process.Unix (forkExecuteFile)

import qualified Data.ByteString.UTF8 as BU

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

lame mpvStdout radioOut = do
  p <- forkExecuteFile
          "lame"
          ["--quiet", "/tmp/pipe.mp3", "/tmp/radio.mp3"]
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
  return p
