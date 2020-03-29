------------------------------------------------------------------------
---IMPORTS
------------------------------------------------------------------------
    -- Base
import XMonad
import XMonad.Config.Desktop
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

    -- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (safeSpawn, spawnPipe)

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doSideFloat, Side(..))

    -- Actions
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.CycleWS (nextScreen, prevScreen)
import XMonad.Actions.GridSelect
import XMonad.Actions.UpdatePointer

    -- Layouts modifiers
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.WindowArranger (WindowArrangerMsg(..))
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, NOBORDERS))
import qualified XMonad.Layout.ToggleLayouts as TL

    -- Layouts
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat
import XMonad.Layout.ZoomRow (zoomReset, ZoomMessage(ZoomFullToggle))

------------------------------------------------------------------------
---CONFIG
------------------------------------------------------------------------
-- Font
myFont :: String
myFont = "xft:monospace:regular:pixelsize=14"

-- Sets modkey to super/windows key
myModMask :: KeyMask
myModMask = mod4Mask

-- Sets default terminal
myTerminal :: String
myTerminal = "alacritty"

-- Sets default text editor
myTextEditor :: String
myTextEditor = "emacsclient -c"

-- Sets default browser
myBrowser :: String
myBrowser = "brave"

-- Sets border width for windows
myBorderWidth :: Dimension
myBorderWidth = 2

-- Gets the number of windows in workspace
windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

main :: IO ()
main = do
  -- Launch xmobar
  xmproc <- spawnPipe "xmobar $HOME/.xmonad/xmobarrc"
  xmonad $ desktopConfig
    { manageHook = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageHook desktopConfig <+> manageDocks
    , logHook = dynamicLogWithPP xmobarPP
      { ppOutput = \x -> hPutStrLn xmproc x
      , ppCurrent = xmobarColor "#BD93F9" "" . wrap "[" "]" -- Current workspace in xmobar
      , ppVisible = xmobarColor "#F8F8F2" ""                -- Visible but not current workspace
      , ppHidden = xmobarColor "#BFBFBF" ""                 -- Hidden workspaces in xmobar
      , ppHiddenNoWindows = xmobarColor "#665C54" ""        -- Hidden workspaces (no windows)
      , ppTitle = xmobarColor "#F8F8F2" "" . shorten 80     -- Title of active window in xmobar
      , ppSep =  "<fc=#F8F8F2> : </fc>"                     -- Separators in xmobar
      , ppUrgent = xmobarColor "#fb4934" "" . wrap "!" "!"  -- Urgent workspace
      , ppExtras  = [windowCount]                           -- # of windows current workspace
      , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
      } >> updatePointer (0.5, 0.5) (0, 0)                  -- Add the pointer follows focus function to logHook
    , modMask            = myModMask
    , terminal           = myTerminal
    , layoutHook         = myLayoutHook
    , workspaces         = myWorkspaces
    , borderWidth        = myBorderWidth
    , normalBorderColor  = "#BFBFBF"
    , focusedBorderColor = "#BD93F9"
    } `additionalKeysP` myKeys
       
------------------------------------------------------------------------
---GRID SELECT
------------------------------------------------------------------------

myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
                  (0x31,0x2e,0x39) -- lowest inactive bg
                  (0x31,0x2e,0x39) -- highest inactive bg
                  (0x61,0x57,0x72) -- active bg
                  (0xc0,0xa7,0x9a) -- inactive fg
                  (0xff,0xff,0xff) -- active fg

-- gridSelect menu layout
myGridConfig :: p -> GSConfig Window
myGridConfig colorizer = (buildDefaultGSConfig myColorizer)
  { gs_cellheight   = 30
  , gs_cellwidth    = 200
  , gs_cellpadding  = 8
  , gs_originFractX = 0.5
  , gs_originFractY = 0.5
  , gs_font         = myFont
  }

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
    where conf = def

------------------------------------------------------------------------
---KEYBINDINGS
------------------------------------------------------------------------
myKeys :: [([Char], X ())]
myKeys =
-- Xmonad
  [ ("M-C-r", spawn "xmonad --recompile")           -- Recompiles xmonad
  , ("M-S-r", spawn "xmonad --restart")             -- Restarts xmonad
  , ("M-S-c", io exitSuccess)                       -- Quits xmonad

-- Windows
  , ("M-S-q", kill1)                                -- Kill the currently focused client
  , ("M-S-a", killAll)                              -- Kill all the windows on current workspace

-- Floating windows
  , ("M-<Delete>", withFocused $ windows . W.sink)  -- Push floating window back to tile.
  , ("M-S-<Delete>", sinkAll)                       -- Push ALL floating windows back to tile.

-- Grid Select
  , ("M-g", goToSelected $ myGridConfig myColorizer)
  , ("M-b", bringSelected $ myGridConfig myColorizer)

-- Windows navigation
  , ("M-m", windows W.focusMaster)                  -- Move focus to the master window
  , ("M-j", windows W.focusDown)                    -- Move focus to the next window
  , ("M-k", windows W.focusUp)                      -- Move focus to the prev window
  , ("M-S-m", windows W.swapMaster)                 -- Swap the focused window and the master window
  , ("M-S-j", windows W.swapDown)                   -- Swap the focused window with the next window
  , ("M-S-k", windows W.swapUp)                     -- Swap the focused window with the prev window
  , ("M-p", promote)                                -- Moves focused window to master, all others maintain order
  , ("M1-<Tab>", rotAllDown)                        -- Rotate all the windows in the current stack
  , ("M1-S-<Tab>", rotSlavesDown)                   -- Rotate all windows except master and keep focus in place

  , ("M-<Up>", sendMessage (MoveUp 10))             --  Move focused window to up
  , ("M-<Down>", sendMessage (MoveDown 10))         --  Move focused window to down
  , ("M-<Right>", sendMessage (MoveRight 10))       --  Move focused window to right
  , ("M-<Left>", sendMessage (MoveLeft 10))         --  Move focused window to left
  , ("M-S-<Up>", sendMessage (IncreaseUp 10))       --  Increase size of focused window up
  , ("M-S-<Down>", sendMessage (IncreaseDown 10))   --  Increase size of focused window down
  , ("M-S-<Right>", sendMessage (IncreaseRight 10)) --  Increase size of focused window right
  , ("M-S-<Left>", sendMessage (IncreaseLeft 10))   --  Increase size of focused window left
  , ("M-C-<Up>", sendMessage (DecreaseUp 10))       --  Decrease size of focused window up
  , ("M-C-<Down>", sendMessage (DecreaseDown 10))   --  Decrease size of focused window down
  , ("M-C-<Right>", sendMessage (DecreaseRight 10)) --  Decrease size of focused window right
  , ("M-C-<Left>", sendMessage (DecreaseLeft 10))   --  Decrease size of focused window left

-- Layouts
  , ("M-<Tab>", sendMessage NextLayout)                                -- Switch to next layout
  , ("M-<Space>", sendMessage ToggleStruts)                            -- Toggles struts
  , ("M-n", sendMessage $ Toggle NOBORDERS)                            -- Toggles noborder
  , ("M-S-f", sendMessage (Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
  , ("M-f", sendMessage TL.ToggleLayout)

  , ("M-h", sendMessage Shrink)
  , ("M-l", sendMessage Expand)
  , ("M-C-j", sendMessage MirrorShrink)
  , ("M-C-k", sendMessage MirrorExpand)
  , ("M-S-;", sendMessage zoomReset)
  , ("M-;", sendMessage ZoomFullToggle)

-- Workspaces
  , ("M-.", nextScreen)                           -- Switch focus to next monitor
  , ("M-,", prevScreen)                           -- Switch focus to prev monitor

-- Open Terminal
  , ("M-<Return>", spawn myTerminal)

-- Dmenu Scripts (Alt+Ctr+Key)
  , ("M-S-<Return>", spawn "dmenu_run -p 'Launch:' -fn 'monospace-11' -nb '#282A36' -nf '#BFBFBF' -sb '#BD93F9' -sf '#E6E6E6'")
  , ("M1-C-u", spawn "dmenuunicode")
  , ("M1-C-m", spawn "dmenumount")
  , ("M1-C-S-m", spawn "dmenuunmount")
  , ("M1-C-t", spawn "torwrap")
  , ("M1-C-S-t", spawn "tortoggle")

-- My Applications (Super+Alt+Key)
  , ("M-M1-i", spawn (myTerminal ++ " -e nmtui"))
  , ("M-M1-l", spawn ("slock"))
  , ("M-M1-h", spawn (myTerminal ++ " -e htop"))
  , ("M-M1-a", spawn (myTerminal ++ " -e pulsemixer"))
  , ("M-M1-p", spawn ("pcmanfm"))
  , ("M-M1-w", spawn (myBrowser))
  , ("M-M1-e", spawn (myTextEditor))
  , ("M-M1-S-e", spawn ("emacs"))
  , ("M-M1-m", spawn ("xrandr --output DP-0 --auto --right-of DP-2"))  -- Turn on second monitor
  , ("M-M1-S-m", spawn ("xrandr --output DP-0 --off"))                 -- Turn off second monitor


-- Multimedia Keys
  -- , ("<XF86AudioPlay>", spawn "cmus toggle")
  -- , ("<XF86AudioPrev>", spawn "cmus prev")
  -- , ("<XF86AudioNext>", spawn "cmus next")
  , ("<XF86AudioMute>",   spawn "pulsemixer --toggle-mute")  -- Bug prevents it from toggling correctly in 12.04.
  , ("<XF86AudioLowerVolume>", spawn "pulsemixer --change-volume -2")
  , ("<XF86AudioRaiseVolume>", spawn "pulsemixer --change-volume +2")
  , ("<XF86HomePage>", spawn myBrowser)
  , ("<XF86Search>", safeSpawn myBrowser ["https://www.duckduckgo.com/"])
  , ("<Print>", spawn "scrotd 0")
  ]


------------------------------------------------------------------------
---WORKSPACES
------------------------------------------------------------------------
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["1","2","3","4","5","6","7","8","9"]

myManageHook :: ManageHook
myManageHook = composeAll
  [ (className =? "Firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
  , className =? "Firefox"             --> doShift "3"--"<action=xdotool key super+1>1</action>"
  , className =? "Brave-browser"       --> doShift "3"
  , className =? "Emacs"               --> doShift "1"
  , className =? "discord"             --> doShift "9"
  , className =? "Pcmanfm"             --> doSideFloat C  -- Spawn window with it's original size centered in the screen
  ]

------------------------------------------------------------------------
---LAYOUTS
------------------------------------------------------------------------
myLayoutHook = avoidStruts $ smartBorders $ TL.toggleLayouts simplestFloat $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ myDefaultLayout
-- avoid xmobar ────┘             |                   |                                         |                         |
-- no border if there's only 1 window                 |                                         |                         |
--                      enable toggling float layout ─┘                                         |                         |
--                                                  enable toggling fullscreen and no borders ──┘                         |
--                                                                                                     custom layout ─────┘

myDefaultLayout = tall ||| noBorders Full ||| simplestFloat
  where
    --tall = spacingRaw False (Border 3 3 3 3) False (Border 3 3 3 3) True $ Tall nmaster delta ratio
    tall = ResizableTall nmaster delta ratio []
    nmaster = 1
    ratio = 1/2
    delta = 3/100
