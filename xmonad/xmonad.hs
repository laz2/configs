
import XMonad
import XMonad.Util.EZConfig
import XMonad.ManageHook
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.NoBorders
import XMonad.Config.Gnome
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageDocks
import XMonad.Actions.CycleWS
import qualified XMonad.StackSet as W

myTerminal = "gnome-terminal"

inTerminal :: [Char] -> [Char]
inTerminal cmd = myTerminal ++ " -e " ++ cmd

myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

myManageHook = composeAll
    [ className  =? "Tilda"   --> doFloat
    , className  =? "MPlayer" --> doFloat
--    , className  =? "Thunderbird" --> moveTo "5"
--    , className  =? "Mail" --> moveTo "5"
    , isFullscreen --> doFullFloat
    ] where moveTo = doF . W.shift

myLayoutHook = tiled ||| Mirror tiled ||| Full
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio   = 1/2

    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

main = xmonad $ gnomeConfig
    { terminal    = myTerminal
    , modMask     = mod4Mask
    , focusFollowsMouse = False
    , borderWidth = 4
    , focusedBorderColor = "#cd8b00"
    , startupHook = setWMName "LG3D"
    , workspaces  = myWorkspaces
--    , layoutHook  = myLayout
--    , manageHook  = myManageHook
    }
    `additionalKeysP` myKeys

myKeys =
  [
    -- rebind meta-p to dmenu
    ("M-p", spawn "exe=`dmenu_path | dmenu -nb '#000000' -nf '#ffffff' -b` && exec $exe")
  , ("M-b", sendMessage ToggleStruts)

  , ("M-s e", spawn "emacs")
  , ("M-s f", spawn "firefox")
  , ("M-s c", spawn "gnome-terminal -e mc")
  , ("M-s a", spawn "gnome-terminal -e alsamixer")
    -- Move focus to the next window
  , ("M-k", windows W.focusDown)

    -- Move focus to the previous window
  , ("M-j", windows W.focusUp)

    -- Swap the focused window with the next window
  , ("M-K", windows W.swapDown)

    -- Swap the focused window with the previous window
  , ("M-J", windows W.swapUp)
  ]
