    -- Base
import XMonad
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

    -- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (nextScreen, prevScreen)
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import qualified XMonad.Actions.Search as S
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WithAll (sinkAll, killAll)

    -- Data
import Data.Char (isSpace, toUpper)
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Monoid
import Data.Tree

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doSideFloat, Side(..))

    -- Layouts
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat

    -- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import qualified XMonad.Layout.ToggleLayouts as T
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.WindowNavigation

    -- Prompt
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Input
import XMonad.Prompt.Man
--import XMonad.Prompt.Pass
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import Control.Arrow (first)

    -- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce


-- Variables and main
-- Font
myFont :: String
myFont = "xft:monospace:regular:size=14:antialias=true::hintingg=true"

-- Sets modkey to super/windows key
myModMask :: KeyMask
myModMask = mod4Mask

-- Sets default terminal
myTerminal :: String
myTerminal = "alacritty"

-- Sets default text editor
myTextEditor :: String
myTextEditor = "emacsclient"

-- Sets default browser
myBrowser :: String
myBrowser = "firefox"

-- Sets border width for windows
myBorderWidth :: Dimension
myBorderWidth = 2

-- Border color of normal windows
myNormColor :: String
myNormColor   = "#BFBFBF"

-- Border color of focused windows
myFocusColor :: String
myFocusColor = "#BD93F9"

-- Gets the number of windows in workspace
windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

main :: IO ()
main = do
  -- Launch xmobar
  xmproc <- spawnPipe "xmobar --iconroot=$HOME/.xmonad/xpm $HOME/.xmonad/xmobarrc"
  -- Xmonad config
  xmonad $ ewmh def
    { manageHook = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageDocks
    , handleEventHook = handleEventHook def
                        <+> fullscreenEventHook
                        <+> docksEventHook
    , modMask            = myModMask
    , terminal           = myTerminal
    , startupHook        = myStartupHook
    , layoutHook         = myLayoutHook
    , workspaces         = myWorkspaces
    , borderWidth        = myBorderWidth
    , normalBorderColor  = myNormColor
    , focusedBorderColor = myFocusColor
    , logHook = dynamicLogWithPP xmobarPP
      { ppOutput = \x -> hPutStrLn xmproc x
      , ppCurrent = xmobarColor "#98BE65" "" . wrap "[" "]" -- Current workspace in xmobar
      , ppVisible = xmobarColor "#98BE65" ""                -- Visible but not current workspace
      , ppHidden = xmobarColor "#82AAFF" ""                 -- Hidden workspaces in xmobar
      , ppHiddenNoWindows = xmobarColor "#8F8C9B" ""        -- Hidden workspaces (no windows)
      , ppTitle = xmobarColor "#D0D0D0" "" . shorten 60     -- Title of active window in xmobar
      , ppSep =  "<fc=#666666> <fn=1>|</fn> </fc>"          -- Separators in xmobar
      , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"  -- Urgent workspace
      , ppExtras  = [windowCount]                           -- # of windows current workspace
      , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
      } >> updatePointer (0.5, 0.5) (0, 0)                  -- Add the pointer follows focus function to logHook
    } `additionalKeysP` myKeys


-- Autostart hook
myStartupHook :: X ()
myStartupHook = do
          spawnOnce "xsetroot -cursor_name left_ptr"
          spawnOnce "nm-applet &"
          spawnOnce "volumeicon &"
          spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 --tint 0x282A36 --height 22 &"


-- Prompts configuration
jdpXPConfig :: XPConfig
jdpXPConfig = def
              { font                = myFont
              , bgColor             = "#282A36"
              , fgColor             = "#F8F8F2"
              , bgHLight            = "#BD93F9"
              , fgHLight            = "#E6E6E6"
              , borderColor         = "#282A36"
              , promptBorderWidth   = 0
              , promptKeymap        = emacsLikeXPKeymap -- Emacs like keybindings for prompt
              , position            = Top
              , height              = 20
              , historySize         = 256
              , historyFilter       = id
              , defaultText         = ""
              , autoComplete        = Just 100000       -- set Just 100000 for .1 sec
              , showCompletionOnTab = False
              , searchPredicate     = fuzzyMatch
              , alwaysHighlight     = True
              , maxComplRows        = Just 5            -- set to 'Just 5' for 5 rows
              }

-- The same config above minus the autocomplete feature which is annoying
-- on certain Xprompts, like the search engine prompts.
jdpXPConfig' :: XPConfig
jdpXPConfig' = jdpXPConfig
               { autoComplete        = Nothing
               }

-- A list of all of the standard Xmonad prompts and a key press assigned to them.
-- These are used in conjunction with a keybinding I set later in the config.
promptList :: [(String, XPConfig -> X ())]
promptList = [ ("m", manPrompt)            -- manpages prompt
             --, ("p", passPrompt)         -- get passwords (requires 'pass')
             --, ("g", passGeneratePrompt) -- generate passwords (requires 'pass')
             --, ("r", passRemovePrompt)   -- remove passwords (requires 'pass')
             , ("r", runOrRaisePrompt)     -- run a program, open a file or raise a running program
             , ("s", sshPrompt)            -- ssh prompt
             , ("x", shellPrompt)          -- shell prompt
             ]

-- This is the list of search engines that I want to use. Some are from
-- XMonad.Actions.Search, and some are the ones that I added above.
searchList :: [(String, S.SearchEngine)]
searchList = [ ("a", archwiki)
             , ("g", S.google)
             , ("y", S.youtube)
             , ("i", S.images)
             , ("w", S.wikipedia)
             , ("z", S.amazon)
             , ("h", S.hoogle)
             , ("b", S.wayback)
             ]
  where
    archwiki = S.searchEngine "archwiki" "https://wiki.archlinux.org/index.php?search="


-- Layout configuration
mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- Layout definition
tall     = renamed [Replace "tall"]
           $ windowNavigation
           $ mySpacing 4
           $ ResizableTall 1 (3/100) (1/2) []
monocle  = renamed [Replace "monocle"]
           $ windowNavigation
           $ Full
floats   = renamed [Replace "floats"]
           $ windowNavigation
           $ simplestFloat

-- Layout hook
myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ smartBorders $ T.toggleLayouts simplestFloat $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ myDefaultLayout
  where
    myDefaultLayout = tall ||| noBorders monocle ||| floats


-- Workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["dev","www","sys","doc","vid","disc"]

myManageHook :: ManageHook
myManageHook = composeAll
  [ (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
  , className =? "firefox"             --> doShift (myWorkspaces !! 1)
  , className =? "Zathura"             --> doShift (myWorkspaces !! 3)
  , className =? "mpv"                 --> doShift (myWorkspaces !! 4)
  , className =? "vlc"                 --> doShift (myWorkspaces !! 4)
  , className =? "discord"             --> doShift (myWorkspaces !! 5)
  , className =? "Pcmanfm"             --> doSideFloat C            -- Spawn window with it's original size centered in the screen
  ]


-- Keybindings
myKeys :: [([Char], X ())]
myKeys =
  -- Xmonad
  [ ("M-x c", spawn "xmonad --recompile")           -- Recompiles xmonad
  , ("M-x r", spawn "xmonad --restart")             -- Restarts xmonad
  , ("M-x q", io exitSuccess)                       -- Quits xmonad
  , ("M-x l", spawn "slock")

  -- Monitors
  , ("M-x m", spawn "xrandr --output DP-0 --auto --right-of DP-2")  -- Turn on second monitor
  , ("M-x M-m", spawn "xrandr --output DP-0 --off")                 -- Turn off second monitor

  -- Windows
  , ("M-q", kill1)                                  -- Kill the currently focused client
  , ("M-S-q", killAll)                              -- Kill all the windows on current workspace

  -- Floating windows
  , ("M-<Delete>", withFocused $ windows . W.sink)  -- Push floating window back to tile.
  , ("M-S-<Delete>", sinkAll)                       -- Push ALL floating windows back to tile.

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
  , ("M-o n", sendMessage NextLayout)                                  -- Switch to next layout
  , ("M-o m", sendMessage ToggleStruts)                                -- Toggles struts
  , ("M-o b", sendMessage $ Toggle NOBORDERS)                          -- Toggles noborder
  , ("M-o f", sendMessage (Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
  , ("M-o t", sendMessage T.ToggleLayout)

  , ("M-h", sendMessage Shrink)
  , ("M-l", sendMessage Expand)
  , ("M-C-j", sendMessage MirrorShrink)
  , ("M-C-k", sendMessage MirrorExpand)

  -- Workspaces
  , ("M-x n", nextScreen)                           -- Switch focus to next monitor
  , ("M-x p", prevScreen)                           -- Switch focus to prev monitor

  -- Open Terminal
  , ("M-<Return>", spawn myTerminal)

  -- Dmenu (currently disabled because I'm trying xmonad prompts)
  --, ("M-<Space>", spawn "dmenu_run -p 'Launch:' -fn 'monospace-11' -nb '#282A36' -nf '#BFBFBF' -sb '#BD93F9' -sf '#E6E6E6'")

  -- My Applications (Super+Alt+Key)
  , ("M-c i", spawn (myTerminal ++ " -e nmtui"))
  , ("M-c h", spawn (myTerminal ++ " -e htop"))
  , ("M-c a", spawn "pavucontrol")
  , ("M-c f", spawn "pcmanfm")
  , ("M-c w", safeSpawn myBrowser [])
  , ("M-c e", spawn myTextEditor)
  , ("M-c M-e", spawn "emacs")

  -- Multimedia Keys
  -- , ("<XF86AudioPlay>", spawn "cmus toggle")
  -- , ("<XF86AudioPrev>", spawn "cmus prev")
  -- , ("<XF86AudioNext>", spawn "cmus next")
  , ("<XF86AudioMute>",   spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
  , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -2%")
  , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +2%")
  , ("<XF86HomePage>", spawn myBrowser)
  , ("<XF86Search>", safeSpawn myBrowser ["https://www.google.com/"])
  , ("<Print>", spawn "scrot '%d-%m-%Y_$wx$h.png' -e 'mv $f ~/Imatges/Captures'")
  ]
  -- Appending search engine prompts to keybindings list.
  -- Look at "search engines" section of this config for values for "k".
  ++ [("M-s " ++ k, S.promptSearch jdpXPConfig' f) | (k,f) <- searchList ]
  ++ [("M-S-s " ++ k, S.selectSearch f) | (k,f) <- searchList ]
  -- Appending some extra xprompts to keybindings list.
  -- Look at "xprompt settings" section this of config for values for "k".
  ++ [("M-<Space> " ++ k, f jdpXPConfig) | (k,f) <- promptList ]
