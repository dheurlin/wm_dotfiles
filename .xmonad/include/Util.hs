module Util where

import Data.Char
import System.IO
import Control.Monad.IO.Class

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

printDebug :: (Show a, MonadIO m) => a -> m ()
printDebug val =
  liftIO $ withFile "/home/sagge/.xmonad/debug" WriteMode $ \file ->
    hPrint file val
