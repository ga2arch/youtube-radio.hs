{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import Control.Monad
import           Control.Monad.STM
import Control.Monad.IO.Class
import           Data.Conduit
import           Data.Conduit.Process.Unix
import           Data.Conduit.TMChan
import           Network.HTTP.Types (status200)
import           Network.Socket.Internal
import           Network.Wai
import           Network.Wai.Handler.Warp
import           System.Environment
import           System.IO
import           System.Process (readProcess)
import System.Random (randomRIO)
import Blaze.ByteString.Builder.Internal.Types (Builder)
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
       Nothing Nothing Nothing (Just $ sinkTBMChan out True) Nothing
  url <- sourceTBMChan out $$ CB.sinkLbs
  return url

ffmpeg out url = do
  h <- openFile "damn" WriteMode
  f <- forkExecuteFile "ffmpeg"
       ["-i", (toStrict . BC.init $ url), "-vn", "-f", "mp3", "-"]
       Nothing Nothing Nothing (Just $ sinkTBMChan out False) (Just $ CL.sinkNull)
  return f
  where
    toStrict = head . BC.toChunks

addClient env info chan =
  modifyMVar_ env (return . Env . M.insert info chan . envClients)

removeClient env info =
  modifyMVar_ env (return . Env . M.delete info . envClients)

app env req = do
  chan <- atomically $ newTBMChan 16
  let info = remoteHost req
  addClient env info chan
  responseSourceBracket
    (return ()) (\_ -> removeClient env info) (\_ -> return (status200, [], sourceTBMChan chan))

sendAll env b =  do
  print "sending"
  clients <- fmap envClients $ readMVar env
  print $ M.keys clients
  mapM_ (\c ->
          atomically $
          writeTBMChan c (Chunk $ BBB.fromByteString b)) $ M.elems clients

radio env out =
  runResourceT $
    sourceTBMChan out
    $= CL.mapM (\b -> liftIO $ (forkIO (sendAll env b)) >> return b)
    $$ CL.sinkNull

queue out = do
  print "START QUEUE"
  yurls <- fmap lines $ readFile "playlist"
  yurl <- pick yurls

  url <- youtubeDl yurl
  unless (BC.isPrefixOf "https" url) $ do
    f <- ffmpeg out url
    _ <- waitForProcess f
    return ()
  queue out
  where
    pick ls = fmap (ls !!) $ randomRIO (0, (length ls - 1))

main = do
  env <- newMVar $ Env M.empty
  out <- atomically $ newTBMChan 16
  --url <- youtubeDl "http://www.youtube.com/watch?v=Ddd3BuEJf38"

  --print (head . BC.toChunks . BC.init $ url)
  --f <- ffmpeg url out

  forkIO $ queue out
  forkIO $ radio env out

  --forkIO $ runResourceT $ sourceTBMChan out $$ CB.sinkFile "test.mp3"
  _ <- run 3000 (app env)
  return ()
