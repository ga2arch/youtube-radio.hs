module Streamer (conduitStreamer) where

import           Blaze.ByteString.Builder.Internal.Types (Builder)
import           Control.Applicative
import           Control.Concurrent
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Data.Conduit
import           Data.Conduit.TMChan
import           Network.HTTP.Types (status200)
import           Network.Socket.Internal
import           Network.Wai
import           Network.Wai.Handler.Warp

import qualified Blaze.ByteString.Builder.ByteString as BBB
import qualified Data.Conduit.List as CL
import qualified Data.Map as M

data Env = Env { envClients :: M.Map SockAddr (TBMChan (Flush Builder)) }

addClient env info chan =
  modifyMVar_ env (return . Env . M.insert info chan . envClients)

removeClient env info =
  modifyMVar_ env (return . Env . M.delete info . envClients)

sendAll env b = do
  chans <- M.elems . envClients <$> readMVar env
  mapM_
    (atomically .
     flip writeTBMChan (Chunk $ BBB.fromByteString b)) chans

app env req = do
  chan <- atomically $ newTBMChan 1024
  let info = remoteHost req

  responseSourceBracket
    (addClient env info chan)
    (\_ -> removeClient env info >> (atomically $ closeTBMChan chan))
    (\_ -> return (status200, [], sourceTBMChan chan))

conduitStreamer port = do
  env <- liftIO $ newMVar $ Env M.empty
  bracketP
    (forkIO $ run port (app env))
    (\id -> killThread id)
    (\_ -> CL.mapM (\b -> liftIO $ sendAll env b >> return b))
