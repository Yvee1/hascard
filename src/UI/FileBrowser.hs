{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module UI.FileBrowser (State, drawUI, handleEvent, theMap) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.List
import Brick.Widgets.FileBrowser
import Control.Exception (displayException, try)
import Control.Monad.IO.Class
import Lens.Micro.Platform
import Parser
import States
import StateManagement
import Recents
import Runners
import UI.BrickHelpers
import qualified UI.Attributes as A
import qualified Graphics.Vty as V

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

drawUI :: FBS -> [Widget Name]
drawUI FBS{_fb=b, _exception'=exc} = [drawException exc, center $ ui <=> help]
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
                    , hCenter $ txt "Esc or q: quit"
                    ]

handleEvent :: BrickEvent Name Event -> EventM Name GlobalState ()
handleEvent (VtyEvent ev) = do
    excep <- use $ fbs.exception'
    b <- use $ fbs.fb
    case (excep, ev) of
      (Just _, _) -> fbs.exception' .= Nothing
      (_, e) -> case e of
        V.EvKey V.KEsc [] | not (fileBrowserIsSearching b) -> popState
        V.EvKey (V.KChar 'q') [] | not (fileBrowserIsSearching b) -> popState
        V.EvKey (V.KChar 'h') [] | not (fileBrowserIsSearching b) -> do
            fbs.showHidden %= not
            eFilter <- Just . entryFilter <$> use (fbs.showHidden)
            fbs.fb .= setFileBrowserEntryFilter eFilter b
        _ -> do
            zoom (fbs.fb) $ handleFileBrowserEvent ev
            b' <- use $ fbs.fb
            case (ev, fileBrowserSelection b') of
                (V.EvKey V.KEnter [], []) -> return ()
                (V.EvKey V.KEnter [], [fileInfo]) -> do
                    let fp = fileInfoFilePath fileInfo
                    fileOrExc <- liftIO (try (readFile fp) :: IO (Either IOError String))
                    case fileOrExc of
                        Left exc -> fbs.exception' ?= displayException exc
                        Right file -> case parseCards file of
                            Left parseError -> fbs.exception' ?= parseError
                            Right result -> do
                                    liftIO $ addRecent fp
                                    zoom css refreshRecents
                                    params <- use parameters
                                    goToState $ parameterState params fp result
                (V.EvKey V.KEnter [], _) -> popState
                _ -> return ()
handleEvent _ = return ()
