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
                    , hCenter $ txt "Esc: quit"
                    ]

handleEvent :: GlobalState -> FBS -> BrickEvent Name Event -> EventM Name (Next GlobalState)
handleEvent gs s@FBS{_fb=b, _exception'=excep} (VtyEvent ev) =
  let update = updateFBS gs
      continue' = continue . update
      halt' = continue . popState in
    case (excep, ev) of
      (Just _, _) -> continue' $ s & exception' .~ Nothing
      (_, e) -> case e of
        V.EvKey V.KEsc [] | not (fileBrowserIsSearching b) ->
            halt' gs
        V.EvKey (V.KChar 'c') [V.MCtrl] | not (fileBrowserIsSearching b) ->
            halt' gs
        V.EvKey (V.KChar 'h') [] | not (fileBrowserIsSearching b) -> let s' = s & showHidden %~ not in
            continue' $ s' & fb .~ setFileBrowserEntryFilter (Just (entryFilter (s' ^. showHidden))) b
        _ -> do
            b' <- handleFileBrowserEvent ev b
            let s' = s & fb .~ b'
            case ev of
                V.EvKey V.KEnter [] ->
                    case fileBrowserSelection b' of
                        [] -> continue' s'
                        [fileInfo] -> do
                          let fp = fileInfoFilePath fileInfo
                          fileOrExc <- liftIO (try (readFile fp) :: IO (Either IOError String))
                          case fileOrExc of
                            Left exc -> continue' (s' & exception' ?~ displayException exc)
                            Right file -> case parseCards file of
                              Left parseError -> continue' (s & exception' ?~ errorBundlePretty parseError)
                              -- Right result -> halt' (s' & parsedCards .~ result & filePath ?~ fp)
                              Right result -> continue =<< liftIO (do
                                      addRecent fp
                                      let gs' = update s'
                                      (gs' `moveToState`) <$> cardsWithOptionsState (update s') result)
                        _ -> halt' gs

                _ -> continue' s'
handleEvent gs _ _ = continue gs
