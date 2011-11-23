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

-- Actions
import XMonad.Actions.SpawnOn

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
        , workspaces = ["1:Shell", "2:Editor", "3:Web", "4:Mail", "5:IRC", "6:General"]
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
myManageHook = manageDocks <+> composeAll
    [ title     =? "mutt"                 --> doF (W.shift "4:Mail")
    , title     =? "irssi"                --> doF (W.shift "5:Chat")
    , className =? "Chromium"             --> doF (W.shift "3:Web")
    , className =? "Firefox-bin"          --> doF (W.shift "4:Web")
    , className =? "Firefox"              --> doF (W.shift "4:Web")
    , className =? "Emacs"                --> doF (W.shift "2:Editor")
    , className =? "GVIM"                 --> doF (W.shift "2:Editor")
    , className =? "Thunar"               --> doF (W.shift "6:General")
    ]
 
manageHook' :: ManageHook
manageHook' = (doF W.swapDown) <+> manageDocks <+> manageHook defaultConfig <+> myManageHook

{- spawn windows on launched workspace (instead of current workspace) -}
myDmenu :: X ()
myDmenu = do
  sp <- mkSpawner
  currentWorkspace <- fmap W.currentTag (gets windowset)
  {- spawnOn sp currentWorkspace "exe=`dmenu_path | dmenu ` && eval \"exec $exe\"" -}
  spawnOn sp currentWorkspace "exe=`IFS=:;lsx $PATH|sort -u|dmenu -fn '-*-dina-medium-r-*-*-10-*-*-*-*-*-*-*' -nb '#000000' -nf '#FFFFFF' -sb '#ffff00' -sf '#000000'` && eval \"exec $exe\""

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
    -- cycle through workspaces
    , ((modMask, xK_e), moveTo Next (WSIs (return $ not . (=="SP") . W.tag)))
    , ((modMask, xK_a), moveTo Prev (WSIs (return $ not . (=="SP") . W.tag)))
    , ((modMask, xK_p), myDmenu) -- %! Launch dmenu
    ]
