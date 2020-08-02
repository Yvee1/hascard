{-# LANGUAGE TemplateHaskell #-}
module UI.CardSelector 
  ( State
  , drawUI
  , handleEvent
  , theMap
  , getRecents
  , getRecentsFile
  , addRecent
  , runCardSelectorUI ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Control.Exception (displayException, try)
import Control.Monad (filterM)
import Control.Monad.IO.Class
import Data.Functor (void)
import Data.List (sort)
import Lens.Micro.Platform
import Parser
import Recents
import System.Environment (lookupEnv)
import System.FilePath ((</>), splitFileName, dropExtension, splitPath, joinPath)
import States
import Types
import UI.Attributes hiding (theMap)
import UI.BrickHelpers
import UI.FileBrowser (runFileBrowserUI)
import UI.Cards (Card, runCardsUI, runCardsWithOptions)
import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import qualified Stack as S
import qualified UI.Attributes as A

runCardSelectorUI :: GlobalState -> IO GlobalState
runCardSelectorUI gs = do
  rs <- getRecents
  let prettyRecents = shortenFilepaths (S.toList rs)
  let options = Vec.fromList (prettyRecents ++ ["Select file from system"])
  let initialState = CSS (L.list () options 1) Nothing rs
  return $ gs `goToState` CardSelectorState initialState

drawUI :: CSS -> [Widget Name]
drawUI s = 
  [ drawException (s ^. exception), drawMenu s ]

title :: Widget Name
title = withAttr titleAttr $ hCenteredStrWrap "Select a deck of flashcards"

drawMenu :: CSS -> Widget Name
drawMenu s = 
  joinBorders $
  center $ 
  withBorderStyle unicodeRounded $
  border $
  hLimitPercent 60 $
  title <=>
  hBorder <=>
  hCenter (drawList s)

drawList :: CSS -> Widget Name
drawList s = vLimit 6  $
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

handleEvent :: GlobalState -> CSS -> BrickEvent Name Event -> EventM Name (Next GlobalState)
handleEvent gs s@CSS{_list=l, _exception=exc} (VtyEvent ev) =
  let update = updateCSS gs
      continue' = continue . update
      halt' = continue . popState in
        case (exc, ev) of
          (Just _, _) -> continue' $ s & exception .~ Nothing
          (_, e) -> case e of
            V.EvKey (V.KChar 'c') [V.MCtrl] -> halt' gs
            V.EvKey V.KEsc [] -> halt' gs

            _ -> do l' <- L.handleListEventVi L.handleListEvent e l
                    let s' = (s & list .~ l') in
                      case e of
                        V.EvKey V.KEnter [] ->
                          case L.listSelectedElement l' of
                            Nothing -> continue' s'
                            Just (_, "Select file from system") -> continue =<< liftIO (runFileBrowser (update s'))
                            Just (i, _) -> do
                                let fp = (s' ^. recents) `S.unsafeElemAt` i
                                fileOrExc <- liftIO (try (readFile fp) :: IO (Either IOError String))
                                case fileOrExc of
                                  Left exc -> continue' (s' & exception ?~ displayException exc)
                                  Right file -> case parseCards file of
                                    Left parseError -> continue' (s' & exception ?~ errorBundlePretty parseError)
                                    Right result -> continue =<< liftIO (do
                                      s'' <- addRecentInternal s' fp
                                      let s''' = s'' & exception .~ Nothing
                                      runCardsWithOptions (update s''') result)
                                      -- return $ update ()
                        _ -> continue' s'

handleEvent gs _ _ = continue gs

runFileBrowser :: GlobalState -> IO GlobalState
runFileBrowser gs = do
  result <- runFileBrowserUI gs
  -- maybe (return s) (\(cards, fp) -> addRecentInternal s fp <* runCardsWithOptions (s^.gs) cards) result
  return result

refreshRecents :: CSS -> IO CSS
refreshRecents s = do
  rs <- getRecents
  let prettyRecents = shortenFilepaths (S.toList rs)
      options       = Vec.fromList (prettyRecents ++ ["Select file from system"])
  return $ s & recents .~ rs
             & list    .~ L.list () options 1

addRecentInternal :: CSS -> FilePath -> IO CSS
addRecentInternal s fp = do
  addRecent fp
  refreshRecents s