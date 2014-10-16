-- Imports:
-------------------------------------------------- Base
import XMonad.Main (xmonad)
import XMonad.Core (terminal, modMask, workspaces,
                    startupHook, handleEventHook,
                    layoutHook, logHook, manageHook,
                    normalBorderColor, focusedBorderColor, borderWidth,
                    spawn)

import XMonad.Config (defaultConfig)
import XMonad.Operations (windows, kill)

import XMonad.Util.Cursor (setDefaultCursor)
import Graphics.X11.Types (mod4Mask, xK_Tab, xK_grave,
                           xK_Alt_L, xK_Alt_R, xK_Super_L, xK_Super_R)
import Graphics.X11.Xlib.Cursor (xC_left_ptr)

import XMonad.Util.Run (safeSpawn)
-------------------------------------------------- Workspaces
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.StackSet (focusUp, focusDown, swapUp, swapDown,
                        greedyView, shift)
import XMonad.ManageHook (doShift, title)

import XMonad.Actions.CycleWS (nextWS, prevWS, shiftToNext, shiftToPrev)
import XMonad.Actions.CycleRecentWS (cycleRecentWS)
import XMonad.Actions.FindEmptyWorkspace (viewEmptyWorkspace,
                                          tagToEmptyWorkspace)
-------------------------------------------------- Windows
import XMonad.Hooks.FadeWindows (fadeWindowsLogHook, fadeWindowsEventHook,
                                 isUnfocused, opacity, opaque)
import XMonad.ManageHook (doFloat)
import XMonad.Hooks.Place (placeHook, smart)

import XMonad.Layout.NoBorders (noBorders)
-- import XMonad.Layout.Spacing (smartSpacing)
-------------------------------------------------- Dzen
import XMonad.Util.Run (spawnPipe)
import XMonad.Hooks.UrgencyHook (withUrgencyHook, NoUrgencyHook(..))
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, dzenPP,
  ppCurrent, ppHidden, ppUrgent, ppHiddenNoWindows,
  ppWsSep, ppSep, ppLayout, ppOutput, ppTitle, ppOrder,
  dzenColor, wrap, shorten)

import XMonad.Hooks.ManageDocks (avoidStruts)
-------------------------------------------------- Utilities
import XMonad.Util.EZConfig (additionalKeysP, additionalKeys)

import XMonad.ManageHook (composeAll, className, (--> -- Github thinks this is a comment
                          ), (=?), (<||>), (<&&>))    -- so we need to shift ) to match
import Data.Functor ((<$>))
import Control.Applicative ((<*>))
import Data.List (elemIndex)

import System.IO (hPutStrLn)
import System.Environment (getEnv)

------- Config variables:

font, foreground, background :: String

font = "-*-terminus-medium-*-normal-*-12-*-*-*-*-*-*-*"

foreground = "#ffa500"
background = "#000000"

-- Files from dzen directory to surround dzen workspace entries with:
workspaceBraces :: (String, String)
workspaceBraces = ("side_l.xbm", "side_r.xbm")

workspaceTags :: [String]
workspaceTags = ["main", "chat", "code", "shell",
                 "v", "vi", "vii", "viii", "ix", "x"]

---- Locations

conkyFile, dzenDir :: String -- $HOME will be automatically prepended to these

conkyFile = "/.xmonad/.conky_dzen" -- Location of conky file for right bar
dzenDir   = "/.xmonad/dzen2/"      -- Location of dzen files for the left bar
-- The trailling "/" matters.

-------- XMonad

main :: IO ()
main = do
  home <- getEnv "HOME"

  let conkyFile' = home ++ conkyFile
  -- dzenRightBar is a wrapped conky that we don't handle.
  spawn $ "conky -qc " ++ conkyFile'
          ++ " | dzen2 -x '866' -y '0' -w '500' -h '13' -ta 'r'"
            ++ " -fg '" ++ foreground ++ "'"
            ++ " -bg '" ++ background ++ "'"
            ++ " -fn "  ++ font
  dzenLeftBar <- spawnPipe $
    "dzen2 -x '0' -y '0' -h '13' -w '866' -ta 'l'"
    ++ " -fg '" ++ foreground ++ "'"
    ++ " -bg '" ++ background ++ "'"
    ++ " -fn "  ++ font


  let dzenDir' = home ++ dzenDir
      -- Curried pure functions, using the dzen directory path
      wrappedIn = workspaceFormat dzenDir'
      withGlyph = addGlyph        dzenDir'

  xmonad . withUrgencyHook NoUrgencyHook
         . ewmh
    $ defaultConfig
    { terminal   = "sakura",
      modMask    = mod4Mask, -- Super as mod key

      -- Hate calling it "myWorkspaces" grumble grumble
      workspaces = workspaceTags,

      focusedBorderColor = foreground,
      normalBorderColor  = background,
      borderWidth = 0,

      -- Set the cursor theme for the root window / desktop
      startupHook = setDefaultCursor xC_left_ptr,

      -- Remove borders, don't overlap with dzen, space windows apart
      layoutHook = avoidStruts . noBorders -- smartSpacing 6 .
                   $ layoutHook defaultConfig,

      -- Reapply transparency rules on any X events
      handleEventHook = fadeWindowsEventHook,

      -- Construct new windows behind older ones
      manageHook = composeAll
                     [ className  =? "MPlayer"     --> doFloat
                     , className  =? "Gimp"        --> doFloat

                     , (className =? "FTL" <||>
                        className =? "brogue")     --> placeHook (smart (0.5, 0.5))

                     , (className =? "Firefox" <||>
                        className =? "dota_linux") --> doShift "main"
                     , (className =? "Xchat"   <||>
                        className =? "Hexchat" <||>
                        title     =? "Friends")    --> doShift "chat"
                     , title      =? "Steam"       --> doShift "ix"
                     , className  =? "Exaile"      --> doShift "x" ],

      -- Window fade rules, set up dzen
      logHook = do
        fadeWindowsLogHook $ composeAll
          [ opaque
          , isUnfocused              --> opacity 0.9

          , (className =? "Vlc" <||>
             className =? "Firefox") --> opaque

          , (className =? "Emacs"  <||>
             className =? "Sakura" <||>
             className =? "Xchat"  <||> className =? "Hexchat")
            <&&> not <$> isUnfocused --> opacity 0.95 ]

        dynamicLogWithPP $ dzenPP
          { ppWsSep  = ""
          , ppCurrent         = "#000000" `wrappedIn` "#ffa500"
          , ppHidden          = clickable <*> ("#dcdcdc" `wrappedIn` "#1a1a1a")
          , ppHiddenNoWindows = clickable <*> ("#404040" `wrappedIn` "#1a1a1a")
          , ppUrgent          = clickable <*> ("#000000" `wrappedIn` "#ff4500")
                                . withGlyph "notice.xbm"

          , ppSep    = "  "

          , ppLayout = dzenColor foreground background .
                       onLeftClick "super+space" .

                       renderImage . (dzenDir' ++) .
                       (\x -> case words x of -- Drop layouthook prefix
                                ["Tall"]           -> "tall.xbm"
                                ["Mirror", "Tall"] -> "mtall.xbm"
                                ["Full"]           -> "full.xbm"
                                _                  -> "grid.xbm")

          , ppTitle  = dzenColor foreground background .
                       onScrollWheelUp   "super+j" .
                       onScrollWheelDown "super+k" .
                       onMiddleClick "super+shift+c" .
                       shorten 100

          , ppOrder  = \(ws:rest) ->
                       (onScrollWheelUp   "super+Left" .
                        onScrollWheelDown "super+Right") ws : rest

          , ppOutput = hPutStrLn dzenLeftBar } }

    `additionalKeysP`

    [ ("M-<Left>",  prevWS),
      ("M-<Right>", nextWS),
      ("M-<Up>",    windows focusUp),
      ("M-<Down>",  windows focusDown),

      ("M-S-<Left>",  shiftToPrev >> prevWS),
      ("M-S-<Right>", shiftToNext >> nextWS),
      ("M-S-<Up>",    windows swapUp),
      ("M-S-<Down>",  windows swapDown),

      ("M-0",   windows . greedyView $ workspaceTags !! 9),
      ("M-S-0", windows . shift      $ workspaceTags !! 9),

      ("M1-<Tab>", cycleRecentWS [xK_Alt_L, xK_Alt_R] xK_Tab xK_grave),
      ("M1-<F4>",  kill),

      ("M-`",   viewEmptyWorkspace),
      ("M-S-`", tagToEmptyWorkspace),

      ("M-p", safeSpawn "/home/ashley/bin/navi-menu" []), -- Personal dmenu
      ("M-q", spawn $ "xmonad --recompile && "
                   ++ "killall conky && "
                   ++ "killall dzen2 && "
                   ++ "xmonad --restart"),
      -- ("M-S-q", safeSpawn "/usr/bin/systemctl" ["poweroff"]) ]
      ("M-S-q", spawn "systemctl poweroff") ]

    `additionalKeys`

    [ ((0, xK_Super_L), return ())   -- Make XMonad capture all super keypresses
    , ((0, xK_Super_R), return ()) ] -- instead of VirtualBox

---- Dzen Helpers

-- Generate dzen format for workspace entries
workspaceFormat :: String -> String -> String -> String -> String
workspaceFormat dzen fg bg = wrap left right
  where left  = "^fg("++ bg ++ ")" ++
                "^i(" ++ dzen ++ leftBrace ++ ")" ++
                "^fg(" ++ fg ++ ")^bg(" ++ bg ++ ")"

        right = "^bg()^fg(" ++ bg ++ ")" ++
                "^i(" ++ dzen ++ rightBrace ++ ")" ++
                "^fg()"

        (leftBrace, rightBrace) = workspaceBraces

-- Add on-click functionality. Requires and uses the 'xdotool' package
xdotool :: Int -> String -> String -> String
xdotool n key = wrap left right
  where left  = "^ca(" ++ show n ++ ",xdotool key " ++ key ++ ")"
        right = "^ca()"

onLeftClick, onMiddleClick         :: String -> String -> String
onScrollWheelUp, onScrollWheelDown :: String -> String -> String

onLeftClick   = xdotool 1
onMiddleClick = xdotool 2
-- onRightClick  = xdotool 3    (Not used)
onScrollWheelUp   = xdotool 4
onScrollWheelDown = xdotool 5

clickable :: String -> String -> String
clickable tag = case elemIndex tag workspaceTags of
                    Just 9  -> onLeftClick $ "super+0"
                    Just n  -> onLeftClick $ "super+" ++ show (n + 1)
                    Nothing -> id

-- Prepend an image symbol to a workspace name
addGlyph :: String -> String -> String -> String
addGlyph dzen glyph = (++ " " ++ renderImage (dzen ++ glyph))

-- Format an image filepath to tell dzen to render it.
renderImage :: String -> String
renderImage = wrap "^i(" ")"
