-- Imports:
-------------------------------------------------- Base
import XMonad.Main (xmonad)
import XMonad.Core (terminal, modMask, workspaces,       
                    layoutHook, logHook, manageHook,     
                    normalBorderColor,focusedBorderColor)

import XMonad.Config (defaultConfig)
import XMonad.Operations (windows)

import Graphics.X11.Types (mod4Mask)
-------------------------------------------------- Workspaces
import XMonad.StackSet (focusUp, focusDown)
import XMonad.Actions.CycleWS (nextWS, prevWS)
-------------------------------------------------- Windows
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Hooks.InsertPosition (insertPosition, Position(End), Focus(Newer))
-------------------------------------------------- Xmobar
import XMonad.Hooks.ManageDocks (avoidStruts)
-------------------------------------------------- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import Data.Monoid ((<>))

-- Config:

workspaceTags :: [String]
workspaceTags = ["web", "irc", "code", "shell"] ++ map show ([5..9] :: [Int])

main :: IO ()
main = xmonad $ defaultConfig
  { terminal   = "sakura",
    modMask    = mod4Mask, -- Super as mod key

    workspaces = workspaceTags,

    normalBorderColor  = "#dddddd",
    focusedBorderColor = "#ffaf00",

  -- Fade out inactive windows
    logHook = fadeInactiveLogHook 0.9,

  -- Construct new windows behind older ones
    manageHook = insertPosition End Newer <> manageHook defaultConfig,

  -- Remove borders, don't overlap with xmobar.
    layoutHook = avoidStruts . noBorders
                 $ layoutHook defaultConfig }
  
  `additionalKeysP` [ ("M-<Left>",  prevWS),
                      ("M-<Right>", nextWS),
                      ("M-<Up>",    windows focusUp),
                      ("M-<Down>",  windows focusDown) ]
