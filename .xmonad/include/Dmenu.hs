{-# LANGUAGE LambdaCase #-}

module Dmenu where

import           Util
import           Vars
import qualified Colors                        as Col

import           XMonad
import           XMonad.Core
import           XMonad.Util.Dmenu              ( menu
                                                , menuArgs
                                                )

import           Data.List
import           Control.Monad

-- | Displays dmenu with a prompt
dmenu :: MonadIO m => String -> [String] -> m String
dmenu prompt = menuArgs "dmenu" $ ["-p", prompt] <> dmenuOpts

-- | Displays dmenu without a prompt
dmenu' :: MonadIO m => [String] -> m String
dmenu' = menuArgs "dmenu" dmenuOpts

dmenuRun :: MonadIO m => m ()
dmenuRun = liftIO $ spawn $
  "/usr/bin/j4-dmenu-desktop "          <>
    "--display-binary "                 <>
    "--dmenu=\"" <> dmenuCmd   <> "\" " <>
    "--term=\""  <> myTerminal <> "\""
 where
  dmenuCmd = "dmenu -i " <> (intercalate " " optsEscapeColors)
  optsEscapeColors =
    [ if head s == '#' then "'" <> s <> "'" else s | s <- dmenuOpts]

dmenuOpts :: [String]
dmenuOpts =
  [ "-sb", Col.accentBg -- selected background color
  , "-fn", font
  ]

confirm :: MonadIO m => String -> m () -> m ()
confirm query m = dmenu query ["yes", "cancel"] >>= \case
  ans | trim ans == "yes" -> m
  a                       -> liftIO (putStrLn a)

