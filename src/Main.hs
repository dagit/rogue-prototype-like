{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Data.Monoid
import           Control.Monad
import           Graphics.Vty as V

import           Brick.Main as M
import           Brick.Types as T
import           Brick.Widgets.Border as B
import           Brick.Widgets.List as L
import           Brick.Widgets.Center as C
import           Brick.AttrMap as A
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
import Brick.Util (fg, on)

type State = (Int, Int)

drawUI :: State -> [Widget ()]
drawUI p = [ui]
  where
  hud   = hLimit 20 $
          vBox [ str "HUD1"
               , str "HUD2"
               , fill ' '
               ]
  mapUI = centerAbout (Location p) mapImg
  ui = border (hud <+> vBorder <+> mapUI)
  mapImg = raw $ vertCat
    [ row | rs <- mapData
          , let row = horizCat $ char defAttr <$> rs
    ]
  maxX = 20
  maxY = 10
  mapData :: [[Char]]
  mapData = [ ts | y <- [0..maxY]
                 , let ts = [ t | x <- [0..maxX]
                            , let t =
                                    if (x,y) == p
                                      then '@'
                                      else if x == 0 ||
                                              x == maxX ||
                                              y == 0 ||
                                              y == maxY
                                             then '#'
                                             else '.'
                            ]
            ]



initialState :: State
initialState = (0,0)

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.blue)
    , (L.listSelectedAttr,    V.blue `on` V.white)
    , (customAttr,            fg V.cyan)
    ]

theApp :: M.App State e ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

appEvent :: State -> T.BrickEvent () e -> T.EventM () (T.Next State)
appEvent l (T.VtyEvent (EvKey KEsc [])) = halt l
appEvent (x,y) (T.VtyEvent (EvKey KDown [])) = M.continue (x,y+1)
appEvent (x,y) (T.VtyEvent (EvKey KUp [])) = M.continue (x,y-1)
appEvent (x,y) (T.VtyEvent (EvKey KLeft [])) = M.continue (x-1,y)
appEvent (x,y) (T.VtyEvent (EvKey KRight [])) = M.continue (x+1,y)
appEvent l _ = M.continue l

main :: IO ()
main = void $ M.defaultMain theApp initialState
