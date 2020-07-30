{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, TemplateHaskell, TupleSections #-}

module UI.FileBrowser (runFileBrowserUI) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.List
import Brick.Widgets.FileBrowser
import Control.Exception (displayException, try)
import Control.Monad.IO.Class
import Lens.Micro.Platform
import Parser
import Types
import UI.BrickHelpers
import qualified UI.Attributes as A
import qualified Graphics.Vty as V

type Event = ()
type Name = ()
data State = State
  { _fb         :: FileBrowser Name
  , _exception  :: Maybe String
  , _cards      :: [Card]
  , _filePath   :: Maybe FilePath
  , _showHidden :: Bool
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

theMap :: AttrMap
theMap = applyAttrMappings
    [ (listSelectedFocusedAttr, V.black `on` V.yellow)
    , (fileBrowserCurrentDirectoryAttr, V.white `on` V.blue)
    , (fileBrowserSelectionInfoAttr, V.white `on` V.blue)
    , (fileBrowserDirectoryAttr, fg V.blue)
    , (fileBrowserBlockDeviceAttr, fg V.magenta)
    , (fileBrowserCharacterDeviceAttr, fg V.green)
    , (fileBrowserNamedPipeAttr, fg V.yellow)
    , (fileBrowserSymbolicLinkAttr, fg V.cyan)
    , (fileBrowserUnixSocketAttr, fg V.red)
    , (fileBrowserSelectedAttr, V.white `on` V.magenta)
    ] A.theMap

drawUI :: State -> [Widget Name]
drawUI State{_fb=b, _exception=exc} = [drawException exc, center $ ui <=> help]
    where
        ui = hCenter $
             vLimit 15 $
             hLimit 50 $
             borderWithLabel (txt "Choose a file") $
             renderFileBrowser True b
        help = padTop (Pad 1) $
               vBox [ hCenter $ txt "Up/Down: select, h: toggle show hidden files"
                    , hCenter $ txt "/: search, Ctrl-C or Esc: cancel search"
                    , hCenter $ txt "Enter: change directory or select file"
                    , hCenter $ txt "Esc: quit"
                    ]

handleEvent :: State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent s@State{_fb=b, _exception=excep} (VtyEvent ev) =
  case (excep, ev) of
    (Just _, _) -> continue $ s & exception .~ Nothing
    (_, e) -> case e of
      V.EvKey V.KEsc [] | not (fileBrowserIsSearching b) ->
          halt s
      V.EvKey (V.KChar 'c') [V.MCtrl] | not (fileBrowserIsSearching b) ->
          halt s
      V.EvKey (V.KChar 'h') [] | not (fileBrowserIsSearching b) -> let s' = s & showHidden %~ not in
          continue $ s' & fb .~ setFileBrowserEntryFilter (Just (entryFilter (s' ^. showHidden))) b
      _ -> do
          b' <- handleFileBrowserEvent ev b
          let s' = s & fb .~ b'
          -- If the browser has a selected file after handling the
          -- event (because the user pressed Enter), shut down.
          case ev of
              V.EvKey V.KEnter [] ->
                  case fileBrowserSelection b' of
                      [] -> continue s'
                      [fileInfo] -> do
                        let fp = fileInfoFilePath fileInfo
                        fileOrExc <- liftIO (try (readFile fp) :: IO (Either IOError String))
                        case fileOrExc of
                          Left exc -> continue (s' & exception ?~ displayException exc)
                          Right file -> case parseCards file of
                            Left parseError -> continue (s & exception ?~ show parseError)
                            Right result -> halt (s' & cards .~ result & filePath ?~ fp)
                      _ -> halt s'

              _ -> continue s'
handleEvent s _ = continue s

runFileBrowserUI :: IO (Maybe ([Card], FilePath))
runFileBrowserUI = do
  browser <- newFileBrowser selectNonDirectories () Nothing
  let filteredBrowser = setFileBrowserEntryFilter (Just (entryFilter False)) browser
  s <- defaultMain app (State filteredBrowser Nothing [] Nothing False)
  let mfp = s ^. filePath
  return $ fmap (s ^. cards,) mfp

entryFilter :: Bool -> FileInfo -> Bool
entryFilter acceptHidden info = fileExtensionMatch "txt" info && (acceptHidden || 
  case fileInfoFilename info of
    ".."    -> True
    '.' : _ -> False
    _       -> True)