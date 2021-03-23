import XMonad
import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import XMonad.Hooks.ManageDocks
import XMonad.Util.Dzen
import Graphics.X11.ExtraTypes.XF86
import XMonad.Util.EZConfig

import Data.Monoid

import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M


myTerminal      = "gnome-terminal"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myClickJustFocuses :: Bool
myClickJustFocuses = True

myBorderWidth   = 1

myModMask       = mod4Mask

myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

myNormalBorderColor  = "#101010"
myFocusedBorderColor = "#ff0000"

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    , ((modm,               xK_Return), spawn "rofi -show run")
    , ((modm,               xK_q     ), kill)
    , ((modm,               xK_space ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Multimedia Keys
    
    , ((0, xF86XK_AudioRaiseVolume), spawn "amixer set Master 5%+ unmute")
    , ((0, xF86XK_AudioLowerVolume), spawn "amixer set Master 5%- unmute")
    , ((0, xF86XK_AudioMute), spawn "amixer set Master toggle")

    , ((0, xF86XK_MonBrightnessUp), spawn "lux -a 10%")
    , ((0, xF86XK_MonBrightnessDown), spawn "lux -s 10%")
   
    , ((0, xF86XK_AudioPlay), spawn "playerctl play-pause")
    , ((0, xF86XK_AudioNext), spawn "playerctl next")
    , ((0, xF86XK_AudioPrev), spawn "playerctl previous")    

    , ((modm,               xK_Tab   ), windows W.focusDown)
    , ((modm,               xK_Right ), windows W.focusDown)
    , ((modm,               xK_Left  ), windows W.focusUp  )
    , ((modm,               xK_m     ), windows W.focusMaster  )
    , ((modm .|. shiftMask, xK_Right ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_Left  ), windows W.swapUp    )
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modm .|. shiftMask, xK_c     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]



myLayout = avoidStruts $ tiled ||| Mirror tiled ||| Full
  where
     tiled   = Tall nmaster delta ratio

     nmaster = 1

     ratio   = 1/2

     delta   = 3/100

myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

myEventHook = mempty

myLogHook = return ()

myStartupHook = do
   spawnOnce "nitrogen --restore &"

main = do
  xmproc <- spawnPipe "xmobar -x 0  ~/.xmobar/xmobarrc"
  xmonad $ docks  defaults

defaults = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }

