{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Conduit.Process.Unix (forkExecuteFile)

import qualified Data.ByteString.UTF8 as BU

mpv url = do
  mpv <- forkExecuteFile
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
  return mpv
