{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.STM
import Control.Concurrent
import Data.Conduit
import Data.Conduit.Process.Unix
import Data.Conduit.TMChan
import GHC.IO.Handle.FD
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import System.IO
import System.Directory
import System.Process (readProcess)
import System.Random (randomRIO)
import System.Posix.Files

import qualified Blaze.ByteString.Builder.ByteString as BBB
import qualified Data.ByteString.Lazy as BL
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.ByteString.UTF8 as BU

--------------------------------------

mpv url stdout = do
  p <- forkExecuteFile
         "mpv"
         ["--no-video",
          "-ao",
          "pcm:file=/dev/stdout",
          "--really-quiet",
          (BU.fromString url)]
         Nothing
         Nothing
         Nothing
         (Just $ sinkTBMChan stdout False)
         Nothing
  return p

lame mpvStdout stdout = do
  p <- forkExecuteFile
          "lame"
          ["--quiet",
           "-",
           "-"]
          Nothing
          Nothing
          (Just $ sourceTBMChan mpvStdout)
          (Just $ sinkTBMChan stdout False)
          Nothing
  return p

--------------------------------------

streamUrl yurl = do
  --tmp <- readProcess "youtube-dl" ["-g", (BU.toString yurl)] []
  --let url = take (length tmp - 1) tmp

  putStrLn (BU.toString yurl)

  mpvOut <- atomically $ newTBMChan 16
  lameOut <- atomically $ newTBMChan 16

--when (url !! 4 /= 's') $ do
  m <- mpv (BU.toString yurl) mpvOut
  l <- lame mpvOut lameOut
  return (m, l, lameOut)

  --return ()

app req = do
  let query = queryString req
  let (Just (Just yurl)) = lookup "yurl" query

  responseSourceBracket
    (streamUrl yurl)
    (\(m, l, _) -> terminateProcess l >> terminateProcess m)
    (\(_, _, lameOut) -> do
      let source = sourceTBMChan lameOut
                   $= CL.map (Chunk . BBB.fromByteString)
      return (status200, [], source))

main = do
  _ <- run 3000 app
  return ()
