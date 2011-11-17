import XMonad
import XMonad.Util.Run(spawnPipe)
import qualified XMonad.StackSet as W -- to shift and float windows
import XMonad.Actions.CycleWS

-- Hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName
import XMonad.Hooks.FadeInactive

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
                    , ppUrgent = xmobarColor "#FF0000" ""
                    , ppTitle = xmobarColor "#FFFF00" ""
                    }
        , borderWidth = 1
        , normalBorderColor  = "#222222"
        , focusedBorderColor = "#99CCFF"
        , workspaces = ["1:Shell", "2:Editor", "3:Web", "4:Mail", "5:General"]
        , terminal  = "urxvtc"
        , keys = \c -> myKeys c `M.union` keys defaultConfig c
        }

-- Layouts
standardLayout = tiled ||| Full ||| Grid
    where tiled     = Tall nmaster delta ratio
          nmaster   = 1
          ratio     = 3/5
          delta     = 3/100

fullLayout = layoutHints(noBorders Full)

myLayout = onWorkspace "2:Editor" fullLayout $
	       onWorkspace "3:Web" Full $
	       standardLayout

-- Float & Window setup
myManageHook :: ManageHook
myManageHook = composeAll . concat $
    [ [ className =? c                      --> doFloat | c <- myFloats ]
    , [ title     =? t                      --> doFloat | t <- myOtherFloats ]
    , [ resource  =? r                      --> doIgnore | r <- myIgnores ]
    , [ className =? "GVIM"                 --> doF (W.shift "2:Editor") ]
    , [ className =? "Thunar"               --> doF (W.shift "5:General") ]
    ]
    where
        myIgnores = ["panel", "trayer", "xfce4-notifyd"]
        myFloats = ["feh"]
        myOtherFloats = ["alsamixer"]
 
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
    , ((modMask, xK_Print),   spawn "scrot -q90 /usr/home/petar/pictures/screenshots/%Y-%m-%d.png")
    , ((modMask .|. controlMask, xK_p ), sendMessage MagnifyMore)
    , ((modMask .|. controlMask, xK_l), sendMessage MagnifyLess)
    , ((modMask .|. controlMask, xK_m), sendMessage Toggle)
    -- cycle through workspaces
    , ((modMask, xK_e), moveTo Next (WSIs (return $ not . (=="SP") . W.tag)))
    , ((modMask, xK_a), moveTo Prev (WSIs (return $ not . (=="SP") . W.tag)))
    , ((modMask, xK_p), spawn "exe=`dmenu_path | dmenu -fn '-*-Inconsolata-medium-r-normal-*-12-*-*-*-*-*-*-*' -nb '#000000' -nf '#FFFFFF' -sb '#ffff00' -sf '#000000' ` && eval \"exec $exe\"") -- %! Launch dmenu
    ]
