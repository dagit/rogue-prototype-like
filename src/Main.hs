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
  ( (<+>)
  , str
  , vLimit
  , hLimit
  , vBox
  , withAttr
  )
import Brick.Util (fg, on)

drawUI :: () -> [Widget ()]
drawUI l = [str "Hello, World!"]

initialState :: ()
initialState = ()

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.blue)
    , (L.listSelectedAttr,    V.blue `on` V.white)
    , (customAttr,            fg V.cyan)
    ]

theApp :: M.App () e ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

appEvent :: () -> T.BrickEvent () e -> T.EventM () (T.Next ())
appEvent l (T.VtyEvent e) = undefined
appEvent l _ = M.continue l

main :: IO ()
main = void $ M.defaultMain theApp initialState
