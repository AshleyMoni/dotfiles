-- Imports:
-------------------------------------------------- Base
import XMonad.Main (xmonad)
import XMonad.Core (terminal, modMask, workspaces,
                    layoutHook, logHook, manageHook,
                    normalBorderColor,focusedBorderColor,
                    spawn)

import XMonad.Config (defaultConfig)
import XMonad.Operations (windows)

import XMonad.Hooks.EwmhDesktops (ewmh)

import Graphics.X11.Types (mod4Mask, xK_Tab, xK_grave,
                           xK_Super_L, xK_Super_R,
                           xK_Alt_L, xK_Alt_R)
-------------------------------------------------- Workspaces
import XMonad.StackSet (focusUp, focusDown, swapUp, swapDown,
                        greedyView, shift)
import XMonad.Actions.CycleWS (nextWS, prevWS, shiftToNext, shiftToPrev)
import XMonad.Actions.CycleRecentWS (cycleRecentWS)
-------------------------------------------------- Windows
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Hooks.InsertPosition (insertPosition, Position(End), Focus(Newer))

import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.Spacing (smartSpacing)
-------------------------------------------------- Dzen
import XMonad.Util.Run (spawnPipe)
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, dzenPP,
  ppCurrent, ppHidden, ppUrgent, ppHiddenNoWindows,
  ppWsSep, ppSep, ppLayout, ppOrder, ppOutput, ppTitle,
  dzenColor, wrap, shorten)

import XMonad.Hooks.ManageDocks (avoidStruts)
-------------------------------------------------- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import Data.Monoid ((<>))

import System.IO (hPutStrLn)
import System.Environment (getEnv)

-------- Config variables:

font = "-*-terminus-medium-*-normal-*-12-*-*-*-*-*-*-*"

foreground = orange
background = black

orange = "#ffa500"
black  = "#000000"

-- Files from dzen directory to surround dzen workspace entries with:
workspaceBraces = ("side_l.xbm", "side_r.xbm")

-- myWorkspaces = ["^i(/home/kron/.xmonad/dzen2/fox.xbm)",        -- main / web
--                 "^i(/home/kron/.xmonad/dzen2/mail.xbm)",       -- chat / comm
--                 "^i(/home/kron/.xmonad/dzen2/code.xbm)",       -- code
--                 "^i(/home/kron/.xmonad/dzen2/arch_10x10.xbm)", -- shell
myWorkspaces = ["main", "chat", "code", "shell",
                "v", "vi", "vii", "viii", "ix", "x"]
               `clickToMoveTo`
               (map show $ [1..9] ++ [0])

  where clickToMoveTo :: [String] -> [String] -> [String]
        clickToMoveTo tags keys = [ onLeftClick ("super+" ++ show i) ws |
                                    (ws,i) <- zip tags keys ]

---- Locations
-- $HOME will be automatically prepended to all these locations.

conkyFile = "/.xmonad/.conky_dzen" -- Location of conky file for right bar
dzenDir   = "/.xmonad/dzen2/"      -- Location of dzen files for the left bar
                                -- The trailling "/" matters.

-------- XMonad

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

  xmonad $ ewmh defaultConfig {
    terminal   = "sakura",
    modMask    = mod4Mask, -- Super as mod key

    workspaces = myWorkspaces,

    focusedBorderColor = foreground,
    normalBorderColor  = background,

  -- Fade out inactive windows, set up xdzen
    logHook = fadeInactiveLogHook 0.9 >>
      dynamicLogWithPP (dzenPP {
          ppWsSep  = ""
        , ppCurrent         = black     `wrappedIn` orange
        , ppHidden          = "#dcdcdc" `wrappedIn` "#1a1a1a"
        , ppHiddenNoWindows = "#404040" `wrappedIn` "#1a1a1a"
        , ppUrgent          = ("#dcdcdc" `wrappedIn` "#f92672") .
                              withGlyph "notice.xbm"

        , ppSep    = "  "

        , ppLayout = dzenColor foreground background .
                     onLeftClick "super+space" .

                     renderImage . (dzenDir' ++) .
                     (\x -> case drop 2 $ words x of
                              ["Tall"]           -> "tall.xbm"
                              ["Mirror", "Tall"] -> "mtall.xbm"
                              ["Full"]           -> "full.xbm"
                              _                  -> "grid.xbm")

        , ppTitle  =  onMiddleClick "super+shift+c" .
                      dzenColor foreground background .
                      shorten 80

        , ppOrder  =  id

        -- , ppOutput =  wrap "^ca(2,xdotool key super+left)" "^ca()" >>= hPutStrLn dzenLeftBar
        , ppOutput =  hPutStrLn dzenLeftBar
       }),

  -- Construct new windows behind older ones
    manageHook = insertPosition End Newer <> manageHook defaultConfig,

  -- Remove borders, don't overlap with dzen, space windows apart
    layoutHook = smartSpacing 6 . avoidStruts . noBorders
                 $ layoutHook defaultConfig }

    `additionalKeysP`

    [ ("M-<Left>",  prevWS),
      ("M-<Right>", nextWS),
      ("M-<Up>",    windows focusUp),
      ("M-<Down>",  windows focusDown),

      ("M-S-<Left>",  shiftToPrev >> prevWS),
      ("M-S-<Right>", shiftToNext >> nextWS),
      ("M-S-<Up>",    windows swapUp),
      ("M-S-<Down>",  windows swapDown),

      ("M-0",   windows . greedyView $ myWorkspaces !! 9),
      ("M-S-0", windows . shift      $ myWorkspaces !! 9),

      ("M-<Tab>",  cycleRecentWS [xK_Super_L, xK_Super_R] xK_Tab xK_grave),
      ("M1-<Tab>", cycleRecentWS [xK_Alt_L, xK_Alt_R] xK_Tab xK_grave),

      ("M-q", spawn $ "xmonad --recompile && "
                   ++ "killall conky && "
                   ++ "killall dzen2 && "
                   ++ "xmonad --restart")]

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
  where left  = "^ca(" ++ (show n) ++ ",xdotool key " ++ key ++ ")"
        right = "^ca()"

onLeftClick   = xdotool 1
onMiddleClick = xdotool 2
onRightClick  = xdotool 3
onScrollWheelUp   = xdotool 4
onScrollWheelDown = xdotool 5

-- Prepend an image symbol to a workspace name
addGlyph :: String -> String -> String -> String
addGlyph dzen glyph = ((renderImage $ dzen ++ glyph) ++)

-- Format an image filepath to tell dzen to render it.
renderImage :: String -> String
renderImage = wrap "^i(" ")"
