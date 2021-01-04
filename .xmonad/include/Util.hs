module Util where

import           XMonad
import           XMonad.Util.Run
import           XMonad.Hooks.ManageHelpers ( doRectFloat )
import qualified XMonad.StackSet as SS

import           Data.Ratio
import           Data.Char
import           System.IO

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

-- Creates a centered RationalRect with the specified width and height
centerRect :: Rational -> Rational -> SS.RationalRect
centerRect w h = SS.RationalRect ((1 - w) / 2) ((1 - h) / 2) w h

-- | Returns a RationalRect which encodes the absolute dimensions specified
-- based on your screen size
absoluteRect :: Integer -> Integer -> X SS.RationalRect
absoluteRect w h = do
  rect <- fmap (screenRect . SS.screenDetail . SS.current) (gets windowset)
  let screen_w = fromIntegral $ rect_width  rect
      screen_h = fromIntegral $ rect_height rect
      wRatio =  w % screen_w
      hRatio =  h % screen_h
  pure $ centerRect wRatio hRatio

doAbsRectFloat :: Integer -> Integer -> ManageHook
doAbsRectFloat w h = doRectFloat =<< liftX (absoluteRect w h)


