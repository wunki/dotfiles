import XMonad
import XMonad.Util.Run(spawnPipe, runInTerm)
import qualified XMonad.StackSet as W -- to shift and float windows
import XMonad.Actions.CycleWS

-- Hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName
import XMonad.Hooks.FadeInactive

-- Actions
import XMonad.Actions.SpawnOn
import XMonad.Actions.WindowGo

-- Layouts
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.DwmStyle
import XMonad.Layout.Magnifier
import XMonad.Layout.StackTile
import XMonad.Layout.LayoutHints

-- Keys
import Graphics.X11.ExtraTypes.XF86
import qualified Data.Map as M

import System.IO
import Data.IORef
import qualified System.IO.UTF8 as UTF8

main = do
    xmobar <- spawnPipe "xmobar"  -- start xmobar
    xmonad $ defaultConfig
        { manageHook = manageHook'
        , modMask = mod4Mask
        , layoutHook = avoidStruts $ myLayout
        , logHook = dynamicLogWithPP $ xmobarPP 
                    { ppOutput = UTF8.hPutStrLn xmobar
                    , ppUrgent = xmobarColor "#586e75" ""
                    , ppTitle = xmobarColor "#93a1a1" ""
                    , ppCurrent = xmobarColor "#268bd2" ""
                    }
        , borderWidth = 1
        , normalBorderColor  = "#586e75"
        , focusedBorderColor = "#93a1a1"
        , workspaces = ["1:Shell", "2:Code", "3:Web", "4:Mail", "5:IRC", "6:General"]
        , terminal  = "urxvtc"
        , keys = \c -> myKeys c `M.union` keys defaultConfig c
        , startupHook = myStartupHook
        }

-- Layouts
standardLayout = tiled ||| Full ||| Grid
    where tiled     = Tall nmaster delta ratio
          nmaster   = 1
          ratio     = 3/5
          delta     = 3/100

fullBorders = noBorders Full

myLayout = onWorkspace "2:Editor" fullBorders $
	   onWorkspace "3:Web" Full $
	   standardLayout

-- Float & Window setup
myManageHook :: ManageHook
myManageHook = manageDocks <+> composeAll
    [ title     =? "mutt"                 --> doF (W.shift "4:Mail")
    , title     =? "irssi"                --> doF (W.shift "5:IRC")
    , className =? "Chromium"             --> doF (W.shift "3:Web")
    , className =? "Firefox-bin"          --> doF (W.shift "3:Web")
    , className =? "Firefox"              --> doF (W.shift "3:Web")
    , className =? "Emacs"                --> doF (W.shift "2:Code")
    , className =? "Emacsclient"          --> doF (W.shift "2:Code")
    , className =? "GVIM"                 --> doF (W.shift "2:Code")
    , className =? "Thunar"               --> doF (W.shift "6:General")
    ]
 
manageHook' :: ManageHook
manageHook' = (doF W.swapDown) <+> manageDocks <+> manageHook defaultConfig <+> myManageHook

myKeys conf@(XConfig {XMonad.modMask = modMask, workspaces = ws}) = M.fromList $
    [ ((0, xF86XK_AudioLowerVolume), spawn "amixer -q sset Master 1-") -- Lower volume
    , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q sset Master 1+") -- Raise volume
    , ((0, xF86XK_AudioMute), spawn "amixer -q sset Master toggle") -- Mute
    , ((0, xF86XK_AudioPlay), spawn "mpc toggle") -- Play/pause
    , ((0, xF86XK_AudioPrev), spawn "mpc prev") -- Previous song
    , ((0, xF86XK_AudioNext), spawn "mpc next") -- Next song
    , ((0, xF86XK_PowerOff),  spawn "sudo /sbin/halt") -- Shutdown
    , ((0, xF86XK_Sleep),     spawn "sudo /usr/sbin/pm-suspend") -- Sleep
    , ((modMask, xK_b),       sendMessage ToggleStruts) -- Hide top bar
    , ((modMask, xK_q),       spawn "killall conky dzen2" >> restart "xmonad" True)
    , ((modMask .|. controlMask, xK_s), spawn "scrot -q90 /home/wunki/Pictures/screenshots/%Y-%m-%d-%H%M%S.png")
    , ((modMask .|. controlMask, xK_p), sendMessage MagnifyMore)
    , ((modMask .|. controlMask, xK_l), sendMessage MagnifyLess)
    , ((modMask .|. controlMask, xK_m), sendMessage Toggle)
    , ((modMask .|. controlMask, xK_w), spawn "chromium")
    , ((modMask .|. controlMask, xK_m), raiseMaybe (runInTerm "-title mutt" "mutt") (title =? "mutt"))
    , ((modMask .|. controlMask, xK_c), raiseMaybe (runInTerm "-title irssi" "irssi") (title =? "irssi"))
    -- cycle through workspaces
    , ((modMask, xK_e), moveTo Next (WSIs (return $ not . (=="SP") . W.tag)))
    , ((modMask, xK_a), moveTo Prev (WSIs (return $ not . (=="SP") . W.tag)))
    , ((modMask, xK_p), spawn "dmenu_run -i -b -fn Ubuntu-12 -nb '#002b36' -nf '#839496' -sb '#073642' -sf '#268bd2' -p '>' ") -- %! Launch dmenu
    ]

-- | Perform an arbitrary action at xmonad startup.
myStartupHook :: X ()
myStartupHook = return ()
