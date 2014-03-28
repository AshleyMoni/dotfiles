-- Imports:
-------------------------------------------------- Base
import XMonad.Main (xmonad)
import XMonad.Core (WindowSet, WorkspaceId,
  terminal, modMask, workspaces,
  layoutHook, logHook, manageHook,
  normalBorderColor,focusedBorderColor)

import XMonad.Config (defaultConfig)
import XMonad.Operations (windows)

import Graphics.X11.Types (mod4Mask)
-------------------------------------------------- Workspaces
import XMonad.StackSet (view, currentTag, focusUp, focusDown)
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
  
  `additionalKeysP` [ ("M-<Left>",  windows focusLeft),
                      ("M-<Right>", windows focusRight),
                      ("M-<Up>",    windows focusUp),
                      ("M-<Down>",  windows focusDown) ]

-- focusLeft moves you to the workspace immediately left, focusRight
-- moves you to the workspace immediately right.

focusLeft :: WindowSet -> WindowSet
focusLeft stack = maybeMove $ lookup (currentTag stack) leftMap
  where maybeMove Nothing  = stack
        maybeMove (Just i) = view i stack

focusRight :: WindowSet -> WindowSet
focusRight stack = maybeMove $ lookup (currentTag stack) rightMap
  where maybeMove Nothing  = stack
        maybeMove (Just i) = view i stack

-- rightMap and leftMap map every workspace to the workspace on its
-- immediate right and left respectively.

rightMap :: [(WorkspaceId, WorkspaceId)]
rightMap = case workspaceTags of
             []     -> []
             (_:tl) -> zip workspaceTags tl

leftMap :: [(WorkspaceId, WorkspaceId)]
leftMap = case workspaceTags of
             []     -> []
             (_:tl) -> zip tl workspaceTags
