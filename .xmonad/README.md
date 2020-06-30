# XMonad config

This config is based around having the `~/.xmonad` be a stack project so that
it won't break if the global Haskell install does.

## Installation

Clone this archive and symlink the `.xmonad` directory to `~/.xmonad`. Then clone
xmonad-git, xmonad-contrib-git and xmobar-git into the same directory. Next, run
`stack install` to install everything. To update XMonad or XMobar, simply
run `stack install` again.

## Repl

To open the XMonad config in GHCI, run `stack ghci xmonad.hs`
