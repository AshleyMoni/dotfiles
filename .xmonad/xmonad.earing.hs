-- Imports:
-------------------------------------------------- Base
import XMonad.Main (xmonad)
import XMonad.Core (terminal, modMask, workspaces,       
                    layoutHook, logHook, manageHook,     
                    normalBorderColor,focusedBorderColor)

import XMonad.Config (defaultConfig)
import XMonad.Operations (windows)
import XMonad.Util.Run (safeSpawn)

import Graphics.X11.Types (mod4Mask)
-------------------------------------------------- Workspaces
import XMonad.StackSet (focusUp, focusDown, swapUp, swapDown)
import XMonad.Actions.CycleWS (nextWS, prevWS, shiftToNext, shiftToPrev)
-------------------------------------------------- Windows
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)
import XMonad.Hooks.InsertPosition (insertPosition, Position(End), Focus(Newer))

import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.Spacing (smartSpacing)
-------------------------------------------------- Xmobar
import XMonad.Util.Run (spawnPipe)
-- import XMonad.Hooks.DynamicLog (dynamicLogWithPP, xmobarPP,
--                                 ppOutput, ppExtras, ppTitle,
--                                 xmobarColor, shorten)
import XMonad.Hooks.DynamicLog

import XMonad.Hooks.ManageDocks (avoidStruts)
-------------------------------------------------- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import Data.Monoid ((<>))
import System.IO (hPutStrLn)

import XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)

-- Config:

workspaceTags :: [String]
workspaceTags = clickable $ ["i"
                ,"ii"   
                ,"iii"  
                ,"iv"   
                ,"v"
                ,"vi"]  
        where clickable l = [ "^ca(1,xdotool key alt+" ++ show (n) ++ ")" ++ ws ++ "^ca()" |
                                (i,ws) <- zip ([1..] :: [Int]) l,
                                let n = i ]

-- workspaceTags :: [String]
-- workspaceTags = ["web", "irc", "code", "shell"] ++ map show ([5..9] :: [Int])

myXmonadBar = "dzen2 -x '0' -y '0' -h '14' -w '500' -ta 'l' -fg '" ++ foreground ++ "' -bg '" ++ background ++ "' -fn " ++ myFont
myStatusBar = "/home/kron/.xmonad/status_bar '" ++ foreground ++ "' '" ++ background ++ "' " ++ myFont

main :: IO ()
main = do
  dzenLeftBar  <- spawnPipe myXmonadBar
  dzenRightBar <- spawnPipe myStatusBar
  -- bar <- spawnPipe "xmobar ~/.xmonad/xmobarrc"
  xmonad $ ewmh defaultConfig {
    terminal   = "sakura",
    modMask    = mod4Mask, -- Super as mod key

    workspaces = workspaceTags,

    normalBorderColor  = "#dddddd",
    focusedBorderColor = "#ffaf00",

  -- Fade out inactive windows, set up xmobar
    logHook = fadeInactiveLogHook 0.9 >>
              dynamicLogWithPP (defaultPP
               {
                  ppCurrent             = dzenColor color15 background    . pad
                , ppVisible             = dzenColor color14 background    . pad
                , ppHidden              = dzenColor color14 background    . pad
                , ppHiddenNoWindows     = dzenColor background background . pad
                , ppWsSep               = ""
                , ppSep                 = "    "
                , ppLayout              = wrap "^ca(1,xdotool key alt+space)" "^ca()" . dzenColor color2 background .
                                (\x -> case x of
                                        "Full"                    -> "^i(/home/kron/.xmonad/dzen2/layout_full.xbm)"
                                        "Spacing 5 ResizableTall" -> "^i(/home/kron/.xmonad/dzen2/layout_tall.xbm)"
                                        "ResizableTall"           -> "^i(/home/kron/.xmonad/dzen2/layout_tall.xbm)"
                                        "SimplestFloat"           -> "^i(/home/kron/.xmonad/dzen2/mouse_01.xbm)"
                                        "Circle"                  -> "^i(/home/kron/.xmonad/dzen2/full.xbm)"
                                        _                         -> "^i(/home/kron/.xmonad/dzen2/grid.xbm)"
                                ) 
--              , ppTitle       =  wrap "^ca(1,xdotool key alt+shift+x)^fg(#D23D3D)^fn(fkp)x ^fn()" "^ca()" . dzenColor foreground background . shorten 40 . pad
                , ppTitle       =  wrap "^ca(1,xdotool key alt+shift+x)" "^ca()" . dzenColor color15 background . shorten 40 . pad
                , ppOrder       =  \(ws:l:t:_) -> [ws,l, t]
                , ppOutput      =  hPutStrLn dzenLeftBar
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

                        ("M-q", mapM_ (uncurry safeSpawn)
                                  [ ("killall", ["xmobar"]),
                                    ("xmonad",  ["--recompile"]),
                                    ("xmonad",  ["--restart"]) ]) ]


myFont		= "-*-terminus-medium-*-normal-*-9-*-*-*-*-*-*-*"
-- EROSION EDIT
background= "#181512"
foreground= "#D6C3B6"
color0=  "#332d29"
color1=  "#8c644c"
color2=  "#746C48"
color3=  "#bfba92"
color4=  "#646a6d"
color5=  "#766782"
color6=  "#4B5C5E"
color7=  "#504339"
color8=  "#817267"
color9=  "#9f7155"
color10= "#9f7155"
color11= "#E0DAAC"
color12= "#777E82"
color13= "#897796"
color14= "#556D70"
color15= "#9a875f"
