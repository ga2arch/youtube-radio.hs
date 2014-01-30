{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import           Blaze.ByteString.Builder.Internal.Types (Builder)
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.STM
import Control.Exception
import           Data.Conduit
import           Data.Conduit.Process.Unix
import           Data.Conduit.TMChan
import           Network.HTTP.Types (status200)
import           Network.Socket.Internal
import           Network.Wai
import           Network.Wai.Handler.Warp
import           System.Random (randomRIO)

import qualified Blaze.ByteString.Builder.ByteString as BBB
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.ByteString.UTF8 as BU
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Map as M

--------------------------------------

data Env = Env { envClients :: M.Map SockAddr (TBMChan (Flush Builder)) }

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

addClient env info chan =
  modifyMVar_ env (return . Env . M.insert info chan . envClients)

removeClient env info =
  modifyMVar_ env (return . Env . M.delete info . envClients)

app env req = do
  chan <- atomically $ newTBMChan 1024
  let info = remoteHost req
  addClient env info chan
  responseSourceBracket
    (return ())
    (\_ -> removeClient env info >> (atomically $ closeTBMChan chan))
    (\_ -> return (status200, [], sourceTBMChan chan))

sendAll env b = do
  --print "sending"
  clients <- fmap envClients $ readMVar env
  --print $ M.keys clients
  mapM_ (\c ->
          atomically $
          writeTBMChan c (Chunk $ BBB.fromByteString b)) $ M.elems clients

radio env out =
  runResourceT $
    sourceTBMChan out
    $= CL.mapM (\b -> liftIO $ sendAll env b >> return b)
    $$ CL.sinkNull

queue' out = do
  print "START QUEUE"
  yurls <- fmap lines $ readFile "playlist"
  yurl <- pick yurls

  url <- youtubeDl yurl
  print url
  unless (BC.isPrefixOf "https" url) $ ffmpeg out url
  queue out
  where
    pick ls = fmap (ls !!) $ randomRIO (0, (length ls - 1))

queue out = catch (queue' out) (\(e ::SomeException) -> queue out)

main = do
  env <- newMVar $ Env M.empty
  out <- atomically $ newTBMChan 1024

  forkIO $ queue out
  forkIO $ radio env out

  _ <- run 8000 (app env)
  return ()
