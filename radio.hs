{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.STM
import Data.Conduit
import Data.Conduit.Process.Unix
import Data.Conduit.TMChan
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import System.IO
import System.Environment
import System.Process (readProcess)

import qualified Blaze.ByteString.Builder.ByteString as BBB
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL

--------------------------------------

ffmpeg url out = do
  f <- forkExecuteFileSource
       "ffmpeg"
       ["-i",
        (BU.fromString url)
       "-vn",
       "-f",
       "mp3",
       "-"]
       Nothing
       Nothing
       Nothing
       (Just $ sinkTBMChan out False)
       Nothing
  return f

--------------------------------------

streamUrl yurl = do
  tmp <- readProcess "youtube-dl" ["-g", yurl] []
  let url = take (length tmp - 1) tmp

  putStrLn url

  mpvOut <- atomically $ newTBMChan 16
  lameOut <- atomically $ newTBMChan 16

  if (url !! 4 == 's')
    then streamUrl yurl
    else do
      m <- mpv url mpvOut
      l <- lame mpvOut
      return ()

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
