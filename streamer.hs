{-# LANGUAGE OverloadedStrings #-}
module Streamer  where

import           Blaze.ByteString.Builder.Internal.Types (Builder)
import           Control.Applicative
import           Control.Concurrent hiding (yield)
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Data.Conduit
import           Data.Conduit.TMChan
import           Network.HTTP.Types (status200, status400)
import           Network.Socket.Internal
import           Network.Wai
import           Network.Wai.Handler.Warp

import qualified Blaze.ByteString.Builder.ByteString as BBB
import qualified Data.ByteString              as B
import qualified Data.Conduit.List as CL
import qualified Data.Map as M

type Clients = M.Map SockAddr (TBMChan (Flush Builder))
type Mounts = M.Map B.ByteString Clients

data Env = Env { envMounts :: Mounts }

addClient env mount info chan = modifyMVar_ env $ \m -> do
  let mounts = envMounts m
  let cls = mounts M.! mount
  let ncls = M.insert info chan cls
  let nmounts = M.insert mount ncls mounts
  return $ Env nmounts

removeClient env mount info = modifyMVar_ env $ \m -> do
  let mounts = envMounts m
  let cls = mounts M.! mount
  let ncls = M.delete info cls
  let nmounts = M.insert mount ncls mounts
  return $ Env nmounts

{-|}
addClient env mount info chan =
  modifyMVar_ env (return . Env . M.insert info chan . ((flip (M.!)) mount) . envMounts)

removeClient env mount info =
  modifyMVar_ env (return . Env . M.delete info . ((flip (M.!)) mount) . envMounts)

sendAll env mount b = do
  chans <- M.elems . ((flip (M.!)) mount) . envMounts <$> readMVar env
  mapM_
    (atomically .
     flip writeTBMChan (Chunk $ BBB.fromByteString b)) chans

app env req = do
  chan <- atomically $ newTBMChan 1024
  let info = remoteHost req
  let path = rawPathInfo req

  if M.member path $ envMounts env
     then stream info chan
     else error404

  where
    stream info chan =
      responseSourceBracket
        (addClient env info chan)
        (\_ -> removeClient env info >> (atomically $ closeTBMChan chan))
        (\_ -> return (status200, [], sourceTBMChan chan))
    error404 = responseSource status400 [] $
              yield $ Chunk $ BBB.fromByteString "Error"

initServer port = do
  env <- liftIO $ newMVar $ Env M.empty
  return $ forkIO $ run port (app env)

conduitStreamer env mount = do
  clients <- liftIO $ newMVar M.empty :: Clients
  bracketP
    (return ())
    (\id -> killThread id)
    (\_ -> CL.mapM (\b -> liftIO $ sendAll env b >> return b)) -}
