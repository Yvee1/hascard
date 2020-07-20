{-# LANGUAGE TemplateHaskell #-}
module UI.CardSelector (runCardSelectorUI, getRecents, getRecentsFile, addRecent) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Control.Exception (displayException, try)
import Control.Monad.IO.Class
import Data.List (sort)
import Lens.Micro.Platform
import Parser
import Stack (Stack)
import System.Environment (lookupEnv)
import System.FilePath ((</>), splitFileName, dropExtension, splitPath, joinPath)
import UI.BrickHelpers
import UI.FileBrowser (runFileBrowserUI)
import UI.Cards (runCardsUI)
import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import qualified Stack as S
import qualified System.Directory as D
import qualified System.IO.Strict as IOS (readFile)

type Event = ()
type Name = ()
data State = State
  { _list       :: L.List Name String
  , _exception  :: Maybe String
  , _recents    :: Stack FilePath
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
  [ drawMenu s <=> drawException s ]

title :: Widget Name
title = withAttr titleAttr $ hCenteredStrWrap "Select a deck of flashcards"

drawMenu :: State -> Widget Name
drawMenu s = 
  joinBorders $
  center $ 
  withBorderStyle unicodeRounded $
  border $
  hLimitPercent 60 $
  title <=>
  hBorder <=>
  hCenter (drawList s)

drawList :: State -> Widget Name
drawList s = vLimit 6  $
             L.renderListWithIndex (drawListElement l) True l
              where l = s ^. list

drawListElement :: L.List Name String -> Int -> Bool -> String -> Widget Name
drawListElement l i selected = hCenteredStrWrapWithAttr (wAttr1 . wAttr2)
  where wAttr1 = if selected then withDefAttr selectedAttr else id
        wAttr2 = if i == length l - 1 then withAttr lastElementAttr else id

drawException :: State -> Widget Name
drawException s = case s ^. exception of
  Nothing -> emptyWidget
  Just exc  -> withAttr exceptionAttr $ strWrap exc

titleAttr :: AttrName
titleAttr = attrName "title"

selectedAttr :: AttrName
selectedAttr = attrName "selected"

lastElementAttr :: AttrName
lastElementAttr = attrName "last element"

exceptionAttr :: AttrName
exceptionAttr = attrName "exception"

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (L.listAttr, V.defAttr)
    , (selectedAttr, fg V.white `V.withStyle` V.underline)
    , (titleAttr, fg V.yellow)
    , (lastElementAttr, fg V.blue)
    , (exceptionAttr, fg V.red) ]

handleEvent :: State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent s@State{_list=l} (VtyEvent e) =
    case e of
        V.EvKey (V.KChar 'c') [V.MCtrl]  -> halt s
        V.EvKey V.KEsc [] -> halt s

        _ -> do l' <- L.handleListEventVi L.handleListEvent e l
                let s' = (s & list .~ l') in
                  case e of
                    V.EvKey V.KEnter [] ->
                      case L.listSelectedElement l' of
                        Nothing -> continue s'
                        Just (_, "Select file from system") -> suspendAndResume $ runFileBrowser s'
                        Just (i, _) -> do
                            let fp = (s' ^. recents) `S.unsafeElemAt` i
                            fileOrExc <- liftIO (try (readFile fp) :: IO (Either IOError String))
                            case fileOrExc of
                              Left exc -> continue (s' & exception ?~ displayException exc)
                              Right file -> case parseCards file of
                                Left parseError -> continue (s' & exception ?~ show parseError)
                                Right result -> suspendAndResume $ do
                                  s'' <- addRecentInternal s' fp
                                  _ <- runCardsUI result
                                  return (s'' & exception .~ Nothing)
                    _ -> continue s'

handleEvent l _ = continue l

runCardSelectorUI :: IO ()
runCardSelectorUI = do
  rs <- getRecents
  let prettyRecents = shortenFilepaths (S.toList rs)
  let options = Vec.fromList (prettyRecents ++ ["Select file from system"])
  let initialState = State (L.list () options 1) Nothing rs
  _ <- defaultMain app initialState
  return () 

getRecents :: IO (Stack FilePath)
getRecents = do
  rf <- getRecentsFile
  exists <- D.doesFileExist rf
  if exists
    then S.fromList . lines <$> IOS.readFile rf
    else return S.empty

maxRecents :: Int
maxRecents = 5

addRecent :: FilePath -> IO ()
addRecent fp = do
  rs <- getRecents
  let rs'  = fp `S.insert` rs 
      rs'' = if S.size rs' <= maxRecents
              then rs'
              else S.removeLast rs'
  writeRecents rs''

addRecentInternal :: State -> FilePath -> IO State
addRecentInternal s fp = do
  addRecent fp
  refreshRecents s

writeRecents :: Stack FilePath -> IO ()
writeRecents stack = do
  file <- getRecentsFile
  writeFile file $ unlines (S.toList stack)

getRecentsFile :: IO FilePath
getRecentsFile = do
  maybeSnap <- lookupEnv "SNAP_USER_DATA"
  xdg <- D.getXdgDirectory D.XdgData "hascard"

  let dir = case maybeSnap of
                Just path | not (null path) -> path
                          | otherwise       -> xdg
                Nothing                     -> xdg
  D.createDirectoryIfMissing True dir

  return (dir </> "recents")

initLast :: [a] -> ([a], a)
initLast [x] = ([], x)
initLast (x:xs) = let (xs', y) = initLast xs
                   in (x:xs', y)

shortenFilepaths :: [FilePath] -> [FilePath]
shortenFilepaths fps = uncurry shortenFilepaths' (unzip (map ((\(pre, fn) -> (pre, dropExtension fn)) . splitFileName) fps))
  where
    shortenFilepaths' prefixes abbreviations =
      let ds = duplicates abbreviations in
        if null ds then abbreviations else
          shortenFilepaths' 
            (flip map (zip [0..] prefixes) (
              \(i, pre) -> if i `elem` ds then
                joinPath (init (splitPath pre)) else pre
            ))
            (flip map (zip [0..] abbreviations) (
              \(i, abbr) -> if i `elem` ds then 
                last (splitPath (prefixes !! i)) ++ abbr
                else abbr) )
          

duplicates :: Eq a => [a] -> [Int]
duplicates = sort . map fst . duplicates' 0 [] []
  where duplicates' _ _    acc []     = acc
        duplicates' i seen acc (x:xs) = duplicates' (i+1) ((i, x) : seen) acc' xs
          where acc' = case (getPairsWithValue x acc, getPairsWithValue x seen) of
                  ([], []) -> acc
                  ([], ys) -> (i, x) : ys ++ acc
                  (_, _)   -> (i, x) : acc
                -- acc' = if getPairsWithValue x seen then (i, x) : acc else acc 

getPairsWithValue :: Eq a => a -> [(Int, a)] -> [(Int, a)]
getPairsWithValue y []       = []
getPairsWithValue y ((i, x):xs)
  | x == y    = (i, x) : getPairsWithValue y xs
  | otherwise = getPairsWithValue y xs

refreshRecents :: State -> IO State
refreshRecents s = do
  rs <- getRecents
  let prettyRecents = shortenFilepaths (S.toList rs)
      options       = Vec.fromList (prettyRecents ++ ["Select file from system"])
  return $ s & recents .~ rs
             & list    .~ L.list () options 1

runFileBrowser :: State -> IO State
runFileBrowser s = do
  result <- runFileBrowserUI
  maybe (return s) (\(cards, fp) -> addRecentInternal s fp <* runCardsUI cards) result