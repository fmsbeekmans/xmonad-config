import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import qualified Data.Map as M

import XMonad.Actions.OnScreen
import XMonad.Actions.Submap
import XMonad.Actions.CopyWindow as C
import XMonad.Hooks.SetWMName
import System.Exit

myModMask = mod5Mask

main = xmonad $ defaultConfig
  { workspaces = myWorkspaces
  , modMask = myModMask
  , startupHook = setWMName "LG3D"
  } `additionalKeys` myKeys

myWorkspaces = ["0", "1", "2", "3", "4", "5", "6", "7"]

myKeys =  stdCommandKeys
       ++ screenKeys
       ++ spawnCommands

stdCommandKeys =
  [ ((myModMask, xK_Escape)                 , io (exitWith ExitSuccess))
  , ((myModMask .|. shiftMask, xK_k)        , C.kill1)
  , ((myModMask .|. shiftMask, xK_a)        , spawn "dmenu run")
    
  , ((myModMask, xK_h)                      , windows W.focusMaster)
  , ((myModMask, xK_t)                      , windows W.focusUp)
  , ((myModMask, xK_n)                      , windows W.focusDown)
    
  , ((myModMask .|. shiftMask, xK_h)        , windows W.swapMaster)
  , ((myModMask .|. shiftMask, xK_t)        , windows W.swapUp)
  , ((myModMask .|. shiftMask, xK_n)        , windows W.swapDown)

  , ((myModMask .|. mod1Mask, xK_t)         , sendMessage Shrink)
  , ((myModMask .|. mod1Mask, xK_n)         , sendMessage Expand)
  ]

screenKeys = 
  [ ((myModMask, xK_semicolon)              , windows (onlyOnScreen 0 "4"))
  , ((myModMask, xK_q)                      , windows (onlyOnScreen 0 "2"))
  , ((myModMask, xK_j)                      , windows (onlyOnScreen 0 "0"))

  , ((myModMask, xK_m)                      , windows (onlyOnScreen 1 "1"))
  , ((myModMask, xK_w)                      , windows (onlyOnScreen 1 "3"))
  , ((myModMask, xK_v)                      , windows (onlyOnScreen 1 "5"))

  , ((myModMask .|. mod1Mask, xK_semicolon), windows (W.greedyView "4"))
  , ((myModMask .|. mod1Mask, xK_q)        , windows (W.greedyView "2"))
  , ((myModMask .|. mod1Mask, xK_j)        , windows (W.greedyView "0"))

  , ((myModMask .|. mod1Mask, xK_m)        , windows (W.greedyView "1"))
  , ((myModMask .|. mod1Mask, xK_w)        , windows (W.greedyView "3"))
  , ((myModMask .|. mod1Mask, xK_v)        , windows (W.greedyView "5"))
    
  , ((myModMask .|. mod2Mask, xK_semicolon), windows (W.shift "4"))
  , ((myModMask .|. mod2Mask, xK_q)        , windows (W.shift "2"))
  , ((myModMask .|. mod2Mask, xK_j)        , windows (W.shift "0"))

  , ((myModMask .|. mod2Mask, xK_m)        , windows (W.shift "1"))
  , ((myModMask .|. mod2Mask, xK_w)        , windows (W.shift "3"))
  , ((myModMask .|. mod2Mask, xK_v)        , windows (W.shift "5"))
    
  , ((myModMask, xK_k)                     , screenWorkspace 0 >>= flip whenJust (windows . W.view))
  , ((myModMask, xK_b)                     , screenWorkspace 1 >>= flip whenJust (windows . W.view))
  ]

spawnCommands =
  [((myModMask, xK_a)                      , submap . M.fromList $
      [ ((0, xK_o)                         , spawn "idea.sh")
      , ((0, xK_e)                         , spawn "emacsclient -c")
      , ((0, xK_u)                         , spawn "subl")
      , ((0, xK_g)                         , spawn "chromium")
      , ((shiftMask, xK_g)                 , spawn "conkeror")	
      , ((0, xK_c)                         , spawn "urxvt")
      , ((0, xK_r)                         , spawn "deluge")
      , ((0, xK_m)                         , spawn "smplayer")

      , ((0, xK_period)                    , spawn "slock")
      ]
    )
  ]
