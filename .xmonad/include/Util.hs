module Util where

import           XMonad.Util.Run

import           Data.Char
import           System.IO
import           Control.Monad.IO.Class

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

printDebug :: (Show a, MonadIO m) => a -> m ()
printDebug val =
  liftIO $ withFile "/home/danielheurlin/.xmonad/debug" WriteMode $ \file ->
    hPrint file val

readProcess :: FilePath -> [String] -> IO String
readProcess fp args = runProcessWithInput fp args ""

-- | Integer division which rounds up
(//) :: Integral a => a -> a -> a
a // b = (a + b - 1) `div` b

