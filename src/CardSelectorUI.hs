module CardSelectorUI (runCardSelectorUI, getRecents, getRecentsFile, addRecent) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Widgets.FileBrowser (FileInfo, fileInfoFilePath)
import FileBrowserUI
import System.FilePath ((</>), takeBaseName)
import qualified System.Directory as D
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import qualified Brick.Widgets.List as L

----------------------- <Basic copy paste> --------------------------

type Event = ()
type Name = ()
type State = L.List Name String

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
title = withAttr titleAttr $ str "Select a card"

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
drawList s = hLimit 11 $
             vLimit 5  $
             L.renderList drawListElement True s

drawListElement :: Bool -> String -> Widget Name
drawListElement selected text = 
  hCenter $
  attr $
  str text
  where attr = if selected then withAttr selectedAttr else id

titleAttr :: AttrName
titleAttr = attrName "title"

selectedAttr :: AttrName
selectedAttr = attrName "selected"

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (L.listAttr,            V.defAttr)
    , (selectedAttr,    fg V.white `V.withStyle` V.underline)
    , (titleAttr, fg V.yellow) ]

handleEvent :: State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent l (VtyEvent e) =
    case e of
        V.EvKey V.KEnter [] -> halt l

        ev -> continue =<< L.handleListEventVi L.handleListEvent ev l
handleEvent l _ = continue l

----------------------- </Basic copy paste> --------------------------

runCardSelectorUI :: IO (Maybe FilePath)
runCardSelectorUI = do
  recents <- getRecents
  let prettyRecents = map takeBaseName recents
  let options = Vec.fromList (prettyRecents ++ ["Select file from system"])

  --                                   listItemHeight
  let initialState = L.list () options 1
  endState <- defaultMain app initialState
  case L.listSelected endState of
    Nothing -> return Nothing
    Just i  -> if i == length recents
                then runFileBrowser
                else return . Just $ recents !! i

runFileBrowser :: IO (Maybe FilePath)
runFileBrowser = do
  mFileInfo <- runFileBrowserUI
  case mFileInfo of
    Nothing -> return Nothing
    Just fileInfo -> return . Just $ fileInfoFilePath fileInfo

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