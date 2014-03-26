-- Imports:
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
-------------------------------------------------- Base
import XMonad.Main (xmonad)
import XMonad.Core (terminal, modMask, workspaces,
                    layoutHook, logHook,
                    normalBorderColor,focusedBorderColor)

import XMonad.Config (defaultConfig)

import Graphics.X11.Types (mod4Mask)
-------------------------------------------------- Utilities
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.NoBorders (noBorders)

-- Config:

main :: IO ()
main = xmonad defaultConfig
  { terminal   = "sakura",
    modMask    = mod4Mask,

    workspaces = ["web", "irc", "code", "shell"]
                 ++ map show ([5..9] :: [Int]),

    normalBorderColor  = "#dddddd",
    focusedBorderColor = "#ffaf00",

  -- Fade out inactive windows
    logHook = fadeInactiveLogHook 0.9,

  -- Remove borders, don't overlap with dzen/xmobar.
    layoutHook = avoidStruts . noBorders
                 $ layoutHook defaultConfig }
