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

import Graphics.X11.Types (mod4Mask, xK_Super_L, xK_Super_R, xK_Tab, xK_grave)
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
  ppOutput, ppTitle, ppCurrent, ppVisible, ppHidden, ppUrgent,
  ppHiddenNoWindows, ppWsSep, ppSep, ppLayout, ppOrder,
  dzenColor, wrap, shorten)

import XMonad.Hooks.ManageDocks (avoidStruts)
-------------------------------------------------- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import Data.Monoid ((<>))
import System.IO (hPutStrLn)

-- Config:

myXmonadBar :: String
myXmonadBar = "dzen2 -x '0' -y '0' -h '13' -w '866' -ta 'l' -fg '"
              ++ foreground ++ "' -bg '" ++ background ++ "' -fn " ++ font
myStatusBar :: String
myStatusBar = "conky -qc /home/kron/.xmonad/.conky_dzen | dzen2 -x '866' -y '0' -w '500' -h '13' -ta 'r' -fg '"
              ++ foreground ++ "' -bg '" ++ background ++ "' -fn " ++ font

myWorkspaces :: [String]
-- myWorkspaces = clickable ["^i(/home/kron/.xmonad/dzen2/fox.xbm)", -- main / web
--                           "^i(/home/kron/.xmonad/dzen2/mail.xbm)", -- chat / comm
--                           "^i(/home/kron/.xmonad/dzen2/code.xbm)", -- code
--                           "^i(/home/kron/.xmonad/dzen2/arch_10x10.xbm)", -- shell
myWorkspaces = clickable ["[main]", "[chat]", "[code]", "[shell]",
                          "v", "vi", "vii", "viii", "ix", "x"]
  where clickable tags = [ "^ca(1,xdotool key super+" ++ show i ++ ")" ++ ws ++ "^ca()" |
                           (i,ws) <- zip ([1..9] ++ [0] :: [Int]) tags ]

main :: IO ()
main = do
  _           <- spawnPipe myStatusBar
  dzenLeftBar <- spawnPipe myXmonadBar

  xmonad $ ewmh defaultConfig {
    terminal   = "sakura",
    modMask    = mod4Mask, -- Super as mod key

    workspaces = myWorkspaces,

    normalBorderColor  = "#000000",
    focusedBorderColor = "#ffa500",

  -- Fade out inactive windows, set up xdzen
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
                , ppWsSep  = ""
                , ppSep    = "  "
                , ppLayout = wrap "^ca(1,xdotool key super+space)" "^ca()" . dzenColor orange background .
                               (\x -> case drop 2 $ words x of
                                        ["Tall"]           -> "^i(/home/kron/.xmonad/dzen2/tall.xbm)"
                                        ["Mirror", "Tall"] -> "^i(/home/kron/.xmonad/dzen2/mtall.xbm)"
                                        ["Full"]           -> "^i(/home/kron/.xmonad/dzen2/full.xbm)"
                                        _                  -> "^i(/home/kron/.xmonad/dzen2/grid.xbm)")
--              , ppTitle  =  wrap "^ca(1,xdotool key super+shift+x)^fg(#D23D3D)^fn(fkp)x ^fn()" "^ca()" . dzenColor foreground background . shorten 40 . pad
                , ppTitle  =  dzenColor orange background . shorten 70
                , ppOrder  =  id
                , ppOutput =  hPutStrLn dzenLeftBar
               }),

  -- Construct new windows behind older ones
    manageHook = insertPosition End Newer <> manageHook defaultConfig,

  -- Remove borders, don't overlap with dzen, space windows apart
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

                        ("M-0",   windows . greedyView $ myWorkspaces !! 9),
                        ("M-S-0", windows . shift      $ myWorkspaces !! 9),

                        ("M-<Tab>", cycleRecentWS [xK_Super_L, xK_Super_R] xK_Tab xK_grave),

                        ("M-q", spawn "xmonad --recompile && killall conky && killall dzen2 && xmonad --restart")]


font :: String
font = "-*-terminus-medium-*-normal-*-12-*-*-*-*-*-*-*"

background :: String
background = "#000000"

foreground :: String
foreground = orange

orange :: String
orange = "#ffa500"
