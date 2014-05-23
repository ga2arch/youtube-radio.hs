{-# LANGUAGE OverloadedStrings #-}

module Radio where

import Data.ByteString (ByteString)
import System.IO.Streams (InputStream, OutputStream)

import qualified Data.ByteString   as B
import qualified Data.ByteString.Char8 as BC
import qualified System.IO.Streams as S

youtubeDl :: String -> IO (Either String ByteString)
youtubeDl url = do 
    (_, out, _, _) <- S.runInteractiveCommand cmd
    res <- S.read out
    case res of
        Just d -> return $ Right d
        Nothing -> return $ Left "Error"
  where
    cmd = "youtube-dl -g " ++ url

ffmpeg :: ByteString -> IO (InputStream ByteString)
ffmpeg url = do
    (_, out, _, _) <- S.runInteractiveProcess cmd args Nothing Nothing
    return out
  where
    cmd = "ffmpeg"
    args = ["-i", (init $ BC.unpack url),
            "-vn", "-f", "mp3",
            "-"]

main = do
    m <- youtubeDl "https://www.youtube.com/watch?v=ReYl3-XWGUI"
    case m of
        Left error -> putStrLn error
        Right url  -> do 
            out <- ffmpeg url
            S.withFileAsOutput "test.mp3" (S.connect out)

