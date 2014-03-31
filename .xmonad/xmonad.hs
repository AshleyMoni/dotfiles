-- Imports:
-------------------------------------------------- Base
import XMonad.Main (xmonad)
import XMonad.Core (terminal, modMask, workspaces,
                    layoutHook, logHook, manageHook,
                    normalBorderColor,focusedBorderColor,
                    spawn)

import XMonad.Config (defaultConfig)
import XMonad.Operations (windows)
-- import XMonad.Util.Run (safeSpawn)

import Graphics.X11.Types (mod4Mask)
-------------------------------------------------- Workspaces
import XMonad.StackSet (focusUp, focusDown, swapUp, swapDown)
import XMonad.Actions.CycleWS (nextWS, prevWS, shiftToNext, shiftToPrev)
-------------------------------------------------- Windows
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Hooks.InsertPosition (insertPosition, Position(End), Focus(Newer))

import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.Spacing (smartSpacing)
-------------------------------------------------- Xmobar
import XMonad.Util.Run (spawnPipe)
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, dzenPP,
  ppOutput, ppTitle, ppCurrent, ppVisible, ppHidden, ppUrgent,
  ppHiddenNoWindows, ppWsSep, ppSep, ppLayout, ppOrder,
  dzenColor, pad, wrap, shorten)

import XMonad.Hooks.ManageDocks (avoidStruts)
-------------------------------------------------- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import Data.Monoid ((<>))
import System.IO (hPutStrLn)

import XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)

-- Config:

-- workspaceTags :: [String]
-- workspaceTags = clickable $ ["i", "ii", "iii", "iv", "v", "vi"]
--   where clickable l = [ "^ca(1,xdotool key super+" ++ show (n) ++ ")" ++ ws ++ "^ca()" |
--                           (i,ws) <- zip ([1..] :: [Int]) l,
--                           let n = i ]

-- workspaceTags :: [String]
-- workspaceTags = ["web", "irc", "code", "shell"] ++ map show ([5..9] :: [Int])

myXmonadBar :: String
myXmonadBar = "dzen2 -x '0' -y '0' -h '13' -w '866' -ta 'l' -fg '" ++ foreground ++ "' -bg '" ++ background ++ "' -fn " ++ myFont
myStatusBar :: String
myStatusBar = "~/.xmonad/status_bar '" ++ foreground ++ "' '" ++ background ++ "' " ++ myFont

main :: IO ()
main = do
  dzenLeftBar  <- spawnPipe myXmonadBar
  _            <- spawnPipe myStatusBar
  -- bar <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
  xmonad $ ewmh defaultConfig {
    terminal   = "sakura",
    modMask    = mod4Mask, -- Super as mod key

    workspaces = let clickable l = [ "^ca(1,xdotool key super+" ++ show (n) ++ ")" ++ ws ++ "^ca()" |
                                     (i,ws) <- zip ([1..] :: [Int]) l,
                                     let n = i ]
                  in clickable ["[web]", "[chat]", "[code]", "[shell]", "v", "vi", "vii", "viii", "ix", "x"],

    normalBorderColor  = "#dddddd",
    focusedBorderColor = "#ffa500",

  -- Fade out inactive windows, set up xmobar
    logHook = fadeInactiveLogHook 0.9 >>
              dynamicLogWithPP (dzenPP {
                  ppCurrent = wrap "^fg(#ffa500)^i(/home/kron/.xmonad/dzen2/side_l.xbm)^fg(#000000)^bg(#ffa500)"
                                   "^bg()^fg(#ffa500)^i(/home/kron/.xmonad/dzen2/side_r.xbm)^fg()"
                , ppVisible = wrap "^fg(#262626)^i(/home/kron/.xmonad/dzen2/side_l.xbm)^fg(#dcdcdc)^bg(#1a1a1a)"
                                   "^bg()^fg(#1a1a1a)^i(/home/kron/.xmonad/dzen2/side_r.xbm)^fg()"
                , ppHidden = wrap "^fg(#1a1a1a)^i(/home/kron/.xmonad/dzen2/side_l.xbm)^fg(#dcdcdc)^bg(#1a1a1a)"
                                  "^bg()^fg(#1a1a1a)^i(/home/kron/.xmonad/dzen2/side_r.xbm)^fg()"
                , ppHiddenNoWindows = wrap "^fg(#1a1a1a)^i(/home/kron/.xmonad/dzen2/side_l.xbm)^fg(#404040)^bg(#1a1a1a)"
                                           "^bg()^fg(#1a1a1a)^i(/home/kron/.xmonad/dzen2/side_r.xbm)^fg()"
                , ppUrgent = wrap "^fg(#f92672)^i(/home/kron/.xmonad/dzen2/side_l.xbm)^fg(#dcdcdc)^bg(#f92672)^i(/home/kron/.xmonad/dzen2/notice.xbm) "
                                  "^bg()^fg(#f92672)^i(/home/kron/.xmonad/dzen2/side_r.xbm)"
                --   ppCurrent         = dzenColor "#ffd700" background  . pad
                -- , ppVisible         = dzenColor "#cd8500" background  . pad
                -- , ppHidden          = dzenColor "#8b5a00" background  . pad
                -- , ppHiddenNoWindows = dzenColor background background . pad
                , ppWsSep  = ""
                , ppSep    = "  "
                , ppLayout = wrap "^ca(1,xdotool key super+space)" "^ca()" . dzenColor orange background .
                                      (\x -> case x of
                                               "SmartSpacing 6 Tall"        -> "^i(/home/kron/.xmonad/dzen2/tall.xbm)"
                                               "SmartSpacing 6 Mirror Tall" -> "^i(/home/kron/.xmonad/dzen2/mtall.xbm)"
                                               "SmartSpacing 6 Full"        -> "^i(/home/kron/.xmonad/dzen2/full.xbm)"
                                               _             -> "^i(/home/kron/.xmonad/dzen2/grid.xbm)")
                                               -- "Full"                    -> "^i(/home/kron/.xmonad/dzen2/layout_full.xbm)"
                                               -- "Spacing 5 ResizableTall" -> "^i(/home/kron/.xmonad/dzen2/layout_tall.xbm)"
                                               -- "ResizableTall"           -> "^i(/home/kron/.xmonad/dzen2/layout_tall.xbm)"
                                               -- "SimplestFloat"           -> "^i(/home/kron/.xmonad/dzen2/mouse_01.xbm)"
                                               -- "Circle"                  -> "^i(/home/kron/.xmonad/dzen2/full.xbm)"

--              , ppTitle  =  wrap "^ca(1,xdotool key super+shift+x)^fg(#D23D3D)^fn(fkp)x ^fn()" "^ca()" . dzenColor foreground background . shorten 40 . pad
                , ppTitle  =  wrap "^ca(1,xdotool key super+shift+x)" "^ca()" . dzenColor orange background . shorten 80 . pad
                , ppOrder  =  id -- \(ws:l:t:_) -> [ws,l,t]
                , ppOutput =  hPutStrLn dzenLeftBar
               }),

              -- dynamicLogWithPP xmobarPP { ppOutput = hPutStrLn bar,
              --                             ppExtras = [battery],
              --                             ppTitle  = xmobarColor "orange" "" . shorten 50},

  -- Construct new windows behind older ones
    manageHook = insertPosition End Newer <> manageHook defaultConfig,

  -- Remove borders, don't overlap with xmobar, space windows apart
    layoutHook = smartSpacing 6 . avoidStruts . noBorders
                 $ layoutHook defaultConfig }

    `additionalKeysP` [ ("M-<Left>",  prevWS),
                        ("M-<Right>", nextWS),
                        ("M-<Up>",    windows focusUp),
                        ("M-<Down>",  windows focusDown),

                        ("M-S-<Left>",  shiftToPrev >> prevWS),
                        ("M-S-<Right>", shiftToNext >> nextWS),
                        ("M-S-<Up>",    windows swapUp),
                        ("M-S-<Down>",  windows swapDown),
                        ("M-q", spawn "xmonad --recompile && killall conky && killall dzen2 && xmonad --restart")]


myFont :: String
myFont = "-*-terminus-medium-*-normal-*-12-*-*-*-*-*-*-*"

background :: String
background = "#000000"

foreground :: String
foreground = orange

orange :: String
orange = "#ffa500"
