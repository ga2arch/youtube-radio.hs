{-# LANGUAGE OverloadedStrings #-}

module Radio where

import Data.ByteString (ByteString)
import System.IO.Streams (InputStream, OutputStream)

import qualified Data.ByteString   as B
import qualified System.IO.Streams as S

youtubeDl :: String -> IO (Either String ByteString)
youtubeDl url = do 
    (_, outS, _, _) <- S.runInteractiveCommand cmd
    res <- S.read outS
    case res of
        Just d -> return $ Right d
        Nothing -> return $ Left "Error"
  where
    cmd ="youtube-dl -g " ++ url


main = do
    url <- youtubeDl "https://www.youtube.com/watch?v=ReYl3-XWGUI"
    print url
