-------------------------------------------------- Base
import XMonad.Main (xmonad)
import XMonad.Core (terminal, modMask, workspaces, layoutHook)

import XMonad.Config (defaultConfig)

import Graphics.X11.Types (mod4Mask)
-------------------------------------------------- Utilities
import XMonad.Layout.NoBorders (noBorders)

main = xmonad defaultConfig
       { 
         terminal = "sakura",
         modMask = mod4Mask,

         workspaces = ["web", "irc", "code", "shell"] ++ map show [5..9],

         layoutHook = noBorders $ layoutHook defaultConfig

         -- normalBorderColor  = "#dddddd",
         -- focusedBorderColor = "#ffaf00"
       }
