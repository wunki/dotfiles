import           XMonad
import           XMonad.Actions.CycleWS
import           XMonad.Prompt
import           XMonad.Prompt.Window
import qualified XMonad.StackSet              as W
import           XMonad.Util.Run              (spawnPipe)
import           XMonad.Util.SpawnOnce        (spawnOnce)

-- Hooks
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)

-- Actions
import           XMonad.Actions.WindowGo

-- Layouts
import           XMonad.Layout.Grid
import           XMonad.Layout.Magnifier
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Gaps
import           XMonad.Layout.Spacing

-- Keys
import qualified Data.Map                     as M
import           Graphics.X11.ExtraTypes.XF86

import           System.IO

main :: IO()
main = do
    xmonad $ ewmh . docks $ defaultConfig
        { manageHook = manageHook'
        , modMask = mod4Mask
        , layoutHook = avoidStruts myLayout
        , borderWidth = 1
        , normalBorderColor  = "#4C566A"
        , focusFollowsMouse = False
        , focusedBorderColor = "#81A1C1"
        , workspaces = ["1:Emacs", "2:Terminals", "3:Browser", "4", "5"]
        , terminal  = "termite"
        , keys = \c -> myKeys c `M.union` keys defaultConfig c
        , startupHook = myStartupHook <+> startupHook defaultConfig
        , handleEventHook = fullscreenEventHook <+> handleEventHook defaultConfig
        }

-- Startup
myStartupHook = do
  spawnOnce "$HOME/bin/polybar-restart"

-- Layouts
myLayout = smartSpacingWithEdge 3 $ tiled ||| noBorders Full ||| Grid
    where tiled     = Tall nmaster delta ratio
          nmaster   = 1
          ratio     = 3/5
          delta     = 3/100

-- Float & Window setup
myManageHook :: ManageHook
myManageHook = manageDocks <+> composeAll
    [ className =? "Chromium"                --> doF (W.shift "3:Browser")
    , className =? "Firefox-beta"            --> doF (W.shift "3:Browser")
    , className =? "Firefox"                 --> doF (W.shift "3:Browser")
    , className =? "Emacs"                   --> doF (W.shift "1:Emacs")
    , className =? "Code"                    --> doF (W.shift "1:Emacs")
    ]

manageHook' :: ManageHook
manageHook' = doF W.swapDown <+> manageDocks <+> manageHook defaultConfig <+> myManageHook

myKeys conf@(XConfig {XMonad.modMask = modMask, workspaces = ws}) = M.fromList $
    [ ((0, xF86XK_AudioLowerVolume), spawn "amixer sset Master 5%-")        -- Lower volume
    , ((0, xF86XK_AudioRaiseVolume), spawn "amixer sset Master 5%+")        -- Raise volume
    , ((0, xF86XK_AudioMute), spawn "amixer -q sset Master toggle")           -- Mute
    , ((0, xF86XK_AudioPlay), spawn "mpc toggle")                             -- Play/pause
    , ((0, xF86XK_AudioPrev), spawn "mpc prev")                               -- Previous song
    , ((0, xF86XK_AudioNext), spawn "mpc next")                               -- Next song
    , ((0, xF86XK_Launch1),   spawn "firefox")                       -- Launch Firefox
    , ((modMask, xK_b), sendMessage ToggleStruts)             -- Hide top bar
    -- Brightness
    , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 5")
    , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 5")
    , ((modMask .|. controlMask, xK_s), spawn "scrot -q90 /home/wunki/pictures/screenshots/%Y-%m-%d-%H%M%S.png")
    , ((modMask .|. controlMask, xK_p), sendMessage MagnifyMore)
    , ((modMask .|. controlMask, xK_l), sendMessage MagnifyLess)
    , ((modMask .|. controlMask, xK_m), sendMessage Toggle)
    , ((modMask .|. controlMask, xK_w), raiseMaybe (spawn "firefox") (className =? "Firefox"))
    , ((modMask .|. controlMask, xK_e), raiseMaybe (spawn "~/bin/em") (className =? "Emacs"))
    , ((modMask .|. controlMask, xK_c), raiseMaybe (spawn "code") (className =? "Code"))
    -- cycle through workspaces
    , ((modMask, xK_n), moveTo Next (WSIs (return $ not . (=="SP") . W.tag)))
    , ((modMask, xK_p), moveTo Prev (WSIs (return $ not . (=="SP") . W.tag)))
    , ((modMask, xK_d), spawn "dmenu_run -i -fn 'xft:Hack:size=8:antialias=true' -nb '#1d1f21' -nf '#c5c8c6' -sb '#1d1f21' -sf '#81a2be' -p '>' ") -- %! Launch dmenu
    , ((modMask, xK_g), windowPromptGoto  defaultXPConfig)
    , ((modMask, xK_c), windowPromptBring defaultXPConfig)
    ]
