{-# LANGUAGE FlexibleContexts #-}

module UI.CardSelector
  ( State
  , drawUI
  , handleEvent
  , theMap
  , getRecents
  , getRecentsFile
  , addRecent ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Control.Exception (displayException, try)
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Lens.Micro.Platform
import Parser
import Recents
import Runners
import States
import StateManagement
import UI.Attributes hiding (theMap)
import UI.BrickHelpers
import qualified Brick.Widgets.List as L
import qualified Graphics.Vty as V
import qualified Stack as S
import qualified UI.Attributes as A

drawUI :: GlobalState -> CSS -> [Widget Name]
drawUI gs s =
  [ drawException (s ^. exception), drawMenu gs s ]

title :: Widget Name
title = withAttr titleAttr $ str "Select a deck of flashcards "

drawMenu :: GlobalState -> CSS -> Widget Name
drawMenu gs s =
  joinBorders $
  center $
  withBorderStyle unicodeRounded $
  border $
  hLimitPercent 60 $
  hCenter title <=>
  hBorder <=>
  hCenter (drawList s)

drawList :: CSS -> Widget Name
drawList s = vLimit (s ^. maxRecentsToShow + 1)  $
             L.renderListWithIndex (drawListElement l) True l
              where l = s ^. list

drawListElement :: L.List Name String -> Int -> Bool -> String -> Widget Name
drawListElement l i selected = hCenteredStrWrapWithAttr (wAttr1 . wAttr2)
  where wAttr1 = if selected then withDefAttr selectedAttr else id
        wAttr2 = if i == length l - 1 then withAttr lastElementAttr else id

lastElementAttr :: AttrName
lastElementAttr = attrName "last element"

theMap :: AttrMap
theMap = applyAttrMappings
    [ (L.listAttr, V.defAttr)
    , (selectedAttr, fg V.white `V.withStyle` V.underline)
    , (titleAttr, fg V.yellow)
    , (lastElementAttr, fg V.blue) ] A.theMap

handleEvent :: BrickEvent Name Event -> EventM Name GlobalState ()
handleEvent (VtyEvent ev) = do
  l <- use $ css.list
  exc <- use $ css.exception
  case (exc, ev) of
    (Just _, _) -> css.exception .= Nothing
    (_, e) -> case e of
      V.EvKey V.KEsc [] -> popState
      V.EvKey (V.KChar 'q') []  -> popState

      _ -> do zoom (css.list) $ L.handleListEventVi L.handleListEvent e
              case e of
                V.EvKey V.KEnter [] -> do
                  selected <- L.listSelectedElement <$> use (css.list)
                  case selected of
                    Just (_, "Select file from system") -> do
                      fbs <- liftIO fileBrowserState
                      goToState fbs
                    Just (i, _) -> do
                        fp <- (`S.unsafeElemAt` i) <$> use (css.recents)
                        fileOrExc <- liftIO (try (readFile fp) :: IO (Either IOError String))
                        case fileOrExc of
                          Left exc -> css.exception ?= displayException exc
                          Right file -> case parseCards file of
                            Left parseError -> css.exception ?= parseError
                            Right result -> do
                              zoom css $ addRecentInternal fp
                              params <- use parameters
                              goToState (parameterState params fp result)
                _ -> return ()

handleEvent _ = return ()

addRecentInternal ::(MonadState CSS m, MonadIO m) => FilePath -> m ()
addRecentInternal fp = do
  liftIO $ addRecent fp
  refreshRecents
