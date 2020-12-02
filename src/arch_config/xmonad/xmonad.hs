-- Imports
    -- Base
import XMonad
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

    -- Actions
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.CycleWS (nextScreen, prevScreen)
import XMonad.Actions.UpdatePointer
import XMonad.Actions.MouseResize

    -- Data
import Data.Char (isSpace, toUpper)
import Data.Monoid
import Data.Maybe (isJust)
import Data.Tree
import qualified Data.Map as M

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.ManageDocks (avoidStruts, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doSideFloat, Side(..))
import XMonad.Hooks.EwmhDesktops

    -- Layouts
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat

    -- Layouts modifiers
import XMonad.Layout.NoBorders
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, NOBORDERS))
import qualified XMonad.Layout.ToggleLayouts as T

    -- Prompt
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Man
--import XMonad.Prompt.Pass
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.XMonad
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
  xmproc <- spawnPipe "xmobar $HOME/.xmonad/xmobarrc.hs"
  -- Xmonad config
  xmonad $ ewmh def
    { manageHook = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageDocks
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
      , ppCurrent = xmobarColor "#BD93F9" "" . wrap "[" "]" -- Current workspace in xmobar
      , ppVisible = xmobarColor "#BD93F9" ""                -- Visible but not current workspace
      , ppHidden = xmobarColor "#F8F8F2" ""                 -- Hidden workspaces in xmobar
      , ppHiddenNoWindows = xmobarColor "#665C54" ""        -- Hidden workspaces (no windows)
      , ppTitle = xmobarColor "#F8F8F2" "" . shorten 80     -- Title of active window in xmobar
      , ppSep =  "<fc=#F8F8F2> : </fc>"                     -- Separators in xmobar
      , ppUrgent = xmobarColor "#FF5555" "" . wrap "!" "!"  -- Urgent workspace
      , ppExtras  = [windowCount]                           -- # of windows current workspace
      , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
      } >> updatePointer (0.5, 0.5) (0, 0)                  -- Add the pointer follows focus function to logHook
    } `additionalKeysP` myKeys


-- Autostart hook
myStartupHook :: X ()
myStartupHook = do
          spawnOnce "xsetroot -cursor_name left_ptr"

-- Prompts configuration
jdpXPConfig :: XPConfig
jdpXPConfig = def
              { font                = myFont
              , bgColor             = "#282c34"
              , fgColor             = "#bbc2cf"
              , bgHLight            = "#c792ea"
              , fgHLight            = "#000000"
              , borderColor         = "#535974"
              , promptBorderWidth   = 0
              , promptKeymap        = dtXPKeymap
              , position            = Top
              , height              = 20
              , historySize         = 256
              , historyFilter       = id
              , defaultText         = []
              , autoComplete        = Just 100000  -- set Just 100000 for .1 sec
              , showCompletionOnTab = False
              -- , searchPredicate     = isPrefixOf
              , searchPredicate     = fuzzyMatch
              , defaultPrompter     = id $ map toUpper  -- change prompt to UPPER
              , alwaysHighlight     = True
              , maxComplRows        = Nothing      -- set to 'Just 5' for 5 rows
              }

-- The same config above minus the autocomplete feature which is annoying
-- on certain Xprompts, like the search engine prompts.
jdpXPConfig' :: XPConfig
jdpXPConfig' = dtXPConfig
               { autoComplete        = Nothing
               }

-- A list of all of the standard Xmonad prompts and a key press assigned to them.
-- These are used in conjunction with a keybinding I set later in the config.
promptList :: [(String, XPConfig -> X ())]
promptList = [ ("m", manPrompt)          -- manpages prompt
             --, ("p", passPrompt)         -- get passwords (requires 'pass')
             --, ("g", passGeneratePrompt) -- generate passwords (requires 'pass')
             --, ("r", passRemovePrompt)   -- remove passwords (requires 'pass')
             , ("s", sshPrompt)          -- ssh prompt
             , ("x", xmonadPrompt)       -- xmonad prompt
             ]

jdpXPKeymap :: M.Map (KeyMask,KeySym) (XP ())
jdpXPKeymap = M.fromList $
              map (first $ (,) controlMask)   -- control + <key>
              [ (xK_z, killBefore)            -- kill line backwards
              , (xK_k, killAfter)             -- kill line forwards
              , (xK_a, startOfLine)           -- move to the beginning of the line
              , (xK_e, endOfLine)             -- move to the end of the line
              , (xK_m, deleteString Next)     -- delete a character foward
              , (xK_b, moveCursor Prev)       -- move cursor backward
              , (xK_f, moveCursor Next)       -- move cursor forward
              , (xK_BackSpace, killWord Prev) -- kill the previous word
              , (xK_y, pasteString)           -- paste a string
              , (xK_g, quit)                  -- quit out of prompt
              , (xK_bracketleft, quit)
              ]
              ++
              map (first $ (,) altMask)       -- meta key + <key>
              [ (xK_BackSpace, killWord Prev) -- kill the prev word
              , (xK_f, moveWord Next)         -- move a word forward
              , (xK_b, moveWord Prev)         -- move a word backward
              , (xK_d, killWord Next)         -- kill the next word
              , (xK_n, moveHistory W.focusUp')   -- move up thru history
              , (xK_p, moveHistory W.focusDown') -- move down thru history
              ]
              ++
              map (first $ (,) 0) -- <key>
              [ (xK_Return, setSuccess True >> setDone True)
              , (xK_KP_Enter, setSuccess True >> setDone True)
              , (xK_BackSpace, deleteString Prev)
              , (xK_Delete, deleteString Next)
              , (xK_Left, moveCursor Prev)
              , (xK_Right, moveCursor Next)
              , (xK_Home, startOfLine)
              , (xK_End, endOfLine)
              , (xK_Down, moveHistory W.focusUp')
              , (xK_Up, moveHistory W.focusDown')
              , (xK_Escape, quit)
              ]

-- This is the list of search engines that I want to use. Some are from
-- XMonad.Actions.Search, and some are the ones that I added above.
searchList :: [(String, S.SearchEngine)]
searchList = [ ("g", S.google)
             , ("h", S.hoogle)
             , ("i", S.images)
             , ("b", S.wayback)
             , ("w", S.wikipedia)
             , ("y", S.youtube)
             , ("z", S.amazon)
             ]

-- Keybindings
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
  , ("M-f", sendMessage T.ToggleLayout)

  , ("M-h", sendMessage Shrink)
  , ("M-l", sendMessage Expand)
  , ("M-C-j", sendMessage MirrorShrink)
  , ("M-C-k", sendMessage MirrorExpand)

  -- Workspaces
  , ("M-.", nextScreen)                           -- Switch focus to next monitor
  , ("M-,", prevScreen)                           -- Switch focus to prev monitor

  -- Open Terminal
  , ("M-<Return>", spawn myTerminal)

  -- Dmenu Scripts (Alt+Ctr+Key)
  , ("M-S-<Return>", spawn "dmenu_run -p 'Launch:' -fn 'monospace-11' -nb '#282A36' -nf '#BFBFBF' -sb '#BD93F9' -sf '#E6E6E6'")
  , ("M1-C-u", spawn "dmenuunicode")
  , ("M1-C-t", spawn "torwrap")
  , ("M1-C-S-t", spawn "tortoggle")

  -- My Applications (Super+Alt+Key)
  , ("M-M1-i", spawn (myTerminal ++ " -e nmtui"))
  , ("M-M1-l", spawn "slock")
  , ("M-M1-h", spawn (myTerminal ++ " -e htop"))
  , ("M-M1-a", spawn (myTerminal ++ " -e pulsemixer"))
  , ("M-M1-p", spawn "pcmanfm")
  , ("M-M1-w", safeSpawn myBrowser [])
  , ("M-M1-e", spawn myTextEditor)
  , ("M-M1-S-e", spawn "emacs")
  , ("M-M1-m", spawn "xrandr --output DP-0 --auto --right-of DP-2")  -- Turn on second monitor
  , ("M-M1-S-m", spawn "xrandr --output DP-0 --off")                 -- Turn off second monitor


  -- Multimedia Keys
  -- , ("<XF86AudioPlay>", spawn "cmus toggle")
  -- , ("<XF86AudioPrev>", spawn "cmus prev")
  -- , ("<XF86AudioNext>", spawn "cmus next")
  , ("<XF86AudioMute>",   spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
  , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -2%")
  , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +2%")
  , ("<XF86HomePage>", spawn myBrowser)
  , ("<XF86Search>", safeSpawn myBrowser ["https://www.duckduckgo.com/"])
  , ("<Print>", spawn "scrot '%d-%m-%Y_$wx$h.png' -e 'mv $f ~/Imatges/Captures'")
  ]


-- Workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["dev","www","sys","doc","vid","disc"]

myManageHook :: ManageHook
myManageHook = composeAll
  [ (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
  , className =? "firefox"             --> doShift (myWorkspaces !! 2)
  , className =? "Zathura"             --> doShift (myWorkspaces !! 4)
  , className =? "mpv"                 --> doShift (myWorkspaces !! 5)
  , className =? "vlc"                 --> doShift (myWorkspaces !! 5)
  , className =? "discord"             --> doShift (myWorkspaces !! 6)
  , className =? "Pcmanfm"             --> doSideFloat C            -- Spawn window with it's original size centered in the screen
  ]

-- Layouts
myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ smartBorders $ T.toggleLayouts simplestFloat $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ myDefaultLayout

myDefaultLayout = tall ||| noBorders Full ||| simplestFloat
  where
    tall = ResizableTall 1 (3/100) (1/2) []
