{-# LANGUAGE TemplateHaskell #-}
module CardSelectorUI (runCardSelectorUI, getRecents, getRecentsFile, addRecent) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Widgets.FileBrowser (FileInfo, fileInfoFilePath)
import Control.Exception (displayException, try)
import Control.Monad.IO.Class
import FileBrowserUI
import Lens.Micro.Platform
import System.FilePath ((</>), takeBaseName)
import Parser
import Types
import FileBrowserUI
import CardUI
import qualified System.Directory as D
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import qualified Brick.Widgets.List as L

----------------------- <Basic copy paste> --------------------------

type Event = ()
type Name = ()
data State = State
  { _list       :: L.List Name String
  , _exception  :: Maybe String
  , _recents    :: [String]
  }

makeLenses ''State

app :: App State Event Name
app = App 
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const theMap
  }

drawUI :: State -> [Widget Name]
drawUI s = 
  [ drawMenu s ]

title :: Widget Name
title = withAttr titleAttr $ str "Select a deck of flashcards"

drawMenu :: State -> Widget Name
drawMenu s = 
  joinBorders $
  center $ 
  withBorderStyle unicodeRounded $
  border $
  -- hLimit 21 $
  hLimitPercent 60 $
  hCenter title <=>
  hBorder <=>
  hCenter (drawList s)

drawList :: State -> Widget Name
drawList s = hLimit 23 $
             vLimit 5  $
             L.renderListWithIndex (drawListElement l) True l
              where l = s ^. list

drawListElement :: L.List Name String -> Int -> Bool -> String -> Widget Name
drawListElement l i selected text = 
  hCenter $
  wAttr1 $
  wAttr2 $
  str text
  where wAttr1 = if selected then withDefAttr selectedAttr else id
        wAttr2 = if i == length l - 1 then withAttr lastElementAttr else id

titleAttr :: AttrName
titleAttr = attrName "title"

selectedAttr :: AttrName
selectedAttr = attrName "selected"

lastElementAttr :: AttrName
lastElementAttr = attrName "last element"

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (L.listAttr,            V.defAttr)
    , (selectedAttr,    fg V.white `V.withStyle` V.underline)
    , (titleAttr, fg V.yellow)
    , (lastElementAttr, fg V.blue) ]

handleEvent :: State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent s@State{_list=l} (VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] -> halt s
        V.EvKey (V.KChar 'q') [] -> halt s

        _ -> do l' <- L.handleListEventVi L.handleListEvent e l
                let s' = (s & list .~ l') in
                  case e of
                    V.EvKey V.KEnter [] ->
                      case L.listSelectedElement l' of
                        Nothing -> continue s'
                        Just (_, "Select file from system") -> suspendAndResume $ 
                                                                  do runFileBrowser
                                                                     return s'
                        Just (i, _) -> do
                            let fp = (s' ^. recents) !! i
                            strOrExc <- liftIO (try (readFile fp) :: IO (Either IOError String))
                            case strOrExc of
                              Left exc -> continue (s' & exception .~ Just (displayException exc))
                              Right str -> case parseCards str of
                                Left parseError -> continue (s' & exception .~ Just (show parseError))
                                Right result -> suspendAndResume $ do
                                  _ <- runCardUI result               
                                  return s'
                    _ -> continue s'

handleEvent l _ = continue l

----------------------- </Basic copy paste> --------------------------

runCardSelectorUI :: IO ()
runCardSelectorUI = do
  recents <- getRecents
  let prettyRecents = map takeBaseName recents
  let options = Vec.fromList (prettyRecents ++ ["Select file from system"])

  --                                   listItemHeight
  let initialState = State (L.list () options 1) Nothing recents
  _ <- defaultMain app initialState
  return () 

getRecents :: IO [FilePath]
getRecents = do
  rf <- getRecentsFile
  exists <- D.doesFileExist rf
  if exists
    then lines <$> readFile rf
    else return []

addRecent :: FilePath -> IO ()
addRecent s = do
  rf <- getRecentsFile
  recents <- getRecents
  if s `elem` recents
    then return () 
    else if length recents < 5
      then appendFile rf s'
      else writeFile rf (unlines (tail recents)) *> appendFile rf s'
        where s' = s ++ "\n"

getRecentsFile :: IO FilePath
getRecentsFile = do
  xdg <- D.getXdgDirectory D.XdgData "hascard"
  D.createDirectoryIfMissing True xdg
  return (xdg </> "recents")

runFileBrowser :: IO ()
runFileBrowser = do
  cards <- runFileBrowserUI
  if length cards > 0
    then runCardUI cards *> return ()
    else return ()