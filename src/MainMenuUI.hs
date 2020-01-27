{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}

module MainMenuUI where

import Brick
import System.Directory (getDirectoryContents)
import Parser
import Data.Map.Strict (Map)
import Data.List
import Data.Char
-- import Data.
import Control.Exception (displayException)
import qualified Data.Map.Strict as M
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import qualified Data.Text as Text
import Brick.Widgets.List
import Brick.Widgets.FileBrowser
import qualified Graphics.Vty as V

type Event = ()
type Name = ()

app :: App (FileBrowser Name) Event Name
app = App 
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const theMap
  }

errorAttr :: AttrName
errorAttr = "error"

theMap :: AttrMap
theMap = attrMap V.defAttr
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
    , (errorAttr, fg V.red)
    ]

-- drawUI :: FileBrowser Name -> [Widget Name]
-- drawUI b = [renderFileBrowser True b]

drawUI :: FileBrowser Name -> [Widget Name]
drawUI b = [center $ ui <=> help]
    where
        ui = hCenter $
             vLimit 15 $
             hLimit 50 $
             borderWithLabel (txt "Choose a file") $
             renderFileBrowser True b
        help = padTop (Pad 1) $
               vBox [ case fileBrowserException b of
                          Nothing -> emptyWidget
                          Just e -> hCenter $ withDefAttr errorAttr $
                                    txt $ Text.pack $ displayException e
                    , hCenter $ txt "Up/Down: select"
                    , hCenter $ txt "/: search, Ctrl-C or Esc: cancel search"
                    , hCenter $ txt "Enter: change directory or select file"
                    , hCenter $ txt "Esc: quit"
                    ]

handleEvent :: FileBrowser Name -> BrickEvent Name Event -> EventM Name (Next (FileBrowser Name))
handleEvent b (VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc [] | not (fileBrowserIsSearching b) ->
            halt b
        V.EvKey (V.KChar 'c') [V.MCtrl] | not (fileBrowserIsSearching b) ->
            halt b
        _ -> do
            b' <- handleFileBrowserEvent ev b
            -- If the browser has a selected file after handling the
            -- event (because the user pressed Enter), shut down.
            case ev of
                V.EvKey V.KEnter [] ->
                    case fileBrowserSelection b' of
                        [] -> continue b'
                        _ -> halt b'
                _ -> continue b'
handleEvent b _ = continue b

runMainMenuUI :: IO FileInfo
runMainMenuUI = do
  browser <- newFileBrowser selectNonDirectories () Nothing
  let filteredBrowser = setFileBrowserEntryFilter (Just (fileExtensionMatch' "txt")) browser
  b <- defaultMain app filteredBrowser
  return $ head (fileBrowserSelection b)

fileExtensionMatch' :: String -> FileInfo -> Bool
fileExtensionMatch' ext i = case fileInfoFileType i of
    Just RegularFile -> ('.' : (toLower <$> ext)) `isSuffixOf` (toLower <$> fileInfoFilename i)
    _ -> True