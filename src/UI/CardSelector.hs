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

shuffledWidget :: Bool -> Widget Name
shuffledWidget shuffled = withAttr shuffledAttr $ str $ 
    if shuffled then "(Shuffled)" else ""

drawMenu :: GlobalState -> CSS -> Widget Name
drawMenu gs s = 
  joinBorders $
  center $ 
  withBorderStyle unicodeRounded $
  border $
  hLimitPercent 60 $
  hCenter (title <+> shuffledWidget (gs^.doShuffle)) <=>
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

handleEvent :: GlobalState -> CSS -> BrickEvent Name Event -> EventM Name (Next GlobalState)
handleEvent gs s@CSS{_list=l, _exception=exc} (VtyEvent ev) =
  let update = updateCSS gs
      continue' = continue . update
      halt' = continue . popState in
        case (exc, ev) of
          (Just _, _) -> continue' $ s & exception .~ Nothing
          (_, e) -> case e of
            V.EvKey V.KEsc [] -> halt' gs
            V.EvKey (V.KChar 's') []  -> continue (gs & doShuffle %~ not)

            _ -> do l' <- L.handleListEventVi L.handleListEvent e l
                    let s' = (s & list .~ l') in
                      case e of
                        V.EvKey V.KEnter [] ->
                          case L.listSelectedElement l' of
                            Nothing -> continue' s'
                            Just (_, "Select file from system") -> 
                              let gs' = update s' in continue =<< (gs' `goToState`) <$> liftIO fileBrowserState
                            Just (i, _) -> do
                                let fp = (s' ^. recents) `S.unsafeElemAt` i
                                fileOrExc <- liftIO (try (readFile fp) :: IO (Either IOError String))
                                case fileOrExc of
                                  Left exc -> continue' (s' & exception ?~ displayException exc)
                                  Right file -> case parseCards file of
                                    Left parseError -> continue' (s' & exception ?~ parseError)
                                    Right result -> continue =<< liftIO (do
                                      s'' <- addRecentInternal s' fp
                                      let gs' = update s''
                                      return (gs' `goToState` parameterState fp result))
                        _ -> continue' s'

handleEvent gs _ _ = continue gs

addRecentInternal :: CSS -> FilePath -> IO CSS
addRecentInternal s fp = do
  addRecent fp
  refreshRecents s