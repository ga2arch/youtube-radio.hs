{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Streamer  where

import           Blaze.ByteString.Builder.Internal.Types (Builder)
import           Control.Applicative
import           Control.Concurrent hiding (yield)
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Data.Conduit
import           Data.Conduit.TMChan
import           Network.HTTP.Types (status200, status400)
import           Network.HTTP.Types.Header (hContentType)
import           Network.Socket.Internal
import           Network.Wai
import           Network.Wai.Handler.Warp

import qualified Blaze.ByteString.Builder.ByteString as BBB
import qualified Data.ByteString              as B
import qualified Data.Conduit.List as CL
import qualified Data.Map as M

type Clients = M.Map SockAddr (TBMChan (Flush Builder))
type Mounts = M.Map B.ByteString Clients

data Env = Env { _envMounts :: Mounts }
makeLenses ''Env

addClient env mount info chan = modifyMVar_ env $ \e ->
  return $ envMounts . at mount . _Just . at info ?~ chan $ e

removeClient env mount info = modifyMVar_ env $ \e ->
  return $ envMounts . at mount . _Just . at info .~ Nothing $ e

addMount env mount = modifyMVar_ env $ \e ->
  return $ envMounts . at mount ?~ M.empty $ e

sendAll env mount b = do
  mounts <- _envMounts <$> readMVar env
  let chans =  M.elems $ mounts M.! mount
  mapM_
    (atomically .
     flip writeTBMChan (Chunk $ BBB.fromByteString b)) chans

app env req = do
  mounts <- _envMounts <$> readMVar env
  let mount = rawPathInfo req
  if mount `elem` (M.keys mounts)
     then stream env mount req
     else return $ error404

stream env mount req = do
  chan <- atomically $ newTBMChan 128
  let info = remoteHost req
  responseSourceBracket
        (addClient env mount info chan)
        (\_ -> removeClient env mount info
               >> (atomically $ closeTBMChan chan))
        (\_ -> return (status200,
                       [(hContentType, "audio/mpeg")],
                       sourceTBMChan chan))

error404 =
 responseSource status400 [] $
              yield $ Chunk $ BBB.fromByteString "No sources"

runStreamer port = do
  env <- newMVar $ Env M.empty
  pid <- forkIO $ run port (app env)
  return (pid, env)

conduitStreamer env mount = do
  liftIO $ addMount env mount
  CL.mapM (\b -> liftIO $ sendAll env mount b >> return b)
