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

addMount env mount = modifyMVar_ env $ \e -> do
  let mounts = envMounts e
  let nmounts = M.insert mount M.empty mounts
  return $ Env nmounts

sendAll env mount b = do
  mounts <- envMounts <$> readMVar env
  let chans =  M.elems $ mounts M.! mount
  mapM_
    (atomically .
     flip writeTBMChan (Chunk $ BBB.fromByteString b)) chans

app env req = do
  mounts <- envMounts <$> readMVar env
  let mount = rawPathInfo req
  if mount `elem` (M.keys mounts)
     then stream env mount req
     else return $ error404

stream env mount req = do
  chan <- atomically $ newTBMChan 1024
  let info = remoteHost req
  responseSourceBracket
        (addClient env mount info chan)
        (\_ -> removeClient env mount info >> (atomically $ closeTBMChan chan))
        (\_ -> return (status200, [], sourceTBMChan chan))

error404 =
 responseSource status400 [] $
              yield $ Chunk $ BBB.fromByteString "No sources"

initServer = do
  env <- newMVar $ Env M.empty
  return (env, app env)

runServer app port = run port app

conduitStreamer env mount = do
  liftIO $ addMount env mount
  bracketP
    (return ())
    (\_ -> return ())
    (\_ -> CL.mapM (\b -> liftIO $ sendAll env mount b >> return b))
