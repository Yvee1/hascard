{-# LANGUAGE FlexibleContexts #-}
module StateManagement where
import Brick
import Control.Monad.IO.Class
import Control.Monad.State.Class
import Control.Monad.State.Lazy (execState)
import Control.Monad (when, (<=<))
import Data.Maybe (fromJust)
import Lens.Micro.Platform
import Recents
import States hiding (cardState)
import Stack hiding (head)
import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec
import qualified Data.Map.Strict as M
import qualified Stack

getMode :: State -> Mode
getMode (MainMenuState     _) = MainMenu
getMode (SettingsState     _) = Settings
getMode (InfoState         _) = Info
getMode (CardSelectorState _) = CardSelector
getMode (FileBrowserState  _) = FileBrowser
getMode (CardsState        _) = Cards
getMode (ParameterState    _) = Parameter

getState :: MonadState GlobalState m => m State
getState = fromJust <$> safeGetState

mms :: Lens' GlobalState MMS
mms = lens (\gs -> mmsCast . fromJust $ M.lookup MainMenu (gs ^. states)) (\gs s -> gs & states %~ M.insert MainMenu (MainMenuState s))
  where mmsCast s@(MainMenuState mms) = mms
        mmsCast _ = error "impossible"

ss :: Lens' GlobalState SS
ss = lens (\gs -> ssCast . fromJust $ M.lookup Settings (gs ^. states)) (\gs s -> gs & states %~ M.insert Settings (SettingsState s))
  where ssCast s@(SettingsState ss) = ss
        ssCast _ = error "impossible"

is :: Lens' GlobalState IS
is = lens (\gs -> isCast . fromJust $ M.lookup Info (gs ^. states)) (\gs s -> gs & states %~ M.insert Info (InfoState s))
  where isCast s@(InfoState ss) = ss
        isCast _ = error "impossible"

cs :: Lens' GlobalState CS
cs = lens (\gs -> csCast . fromJust $ M.lookup Cards (gs ^. states)) (\gs s -> gs & states %~ M.insert Cards (CardsState s))
  where csCast s@(CardsState cs) = cs
        csCast _ = error "impossible"

css :: Lens' GlobalState CSS
css = lens (\gs -> cssCast . fromJust $ M.lookup CardSelector (gs ^. states)) (\gs s -> gs & states %~ M.insert CardSelector (CardSelectorState s))
  where cssCast s@(CardSelectorState css) = css
        cssCast _ = error "impossible"

fbs :: Lens' GlobalState FBS
fbs = lens (\gs -> fbsCast . fromJust $ M.lookup FileBrowser (gs ^. states)) (\gs s -> gs & states %~ M.insert FileBrowser (FileBrowserState s))
  where fbsCast s@(FileBrowserState fbs) = fbs
        fbsCast _ = error "impossible"

ps :: Lens' GlobalState PS
ps = lens (\gs -> psCast . fromJust $ M.lookup Parameter (gs ^. states)) (\gs s -> gs & states %~ M.insert Parameter (ParameterState s))
  where psCast s@(ParameterState ps) = ps
        psCast _ = error "impossible"

goToState_ :: GlobalState -> State -> GlobalState
goToState_ gs s = execState (goToState s) gs

goToState :: MonadState GlobalState m => State -> m ()
goToState s = do states %= M.insert (getMode s) s
                 stack  %= insert (getMode s)

moveToState :: MonadState GlobalState m => State -> m ()
moveToState s = do
  popState
  goToState s

-- popState until at mode of state s.
removeToState :: MonadState GlobalState m => State -> m ()
removeToState s = do
  popState
  current <- Stack.head <$> use stack
  if current == getMode s 
    then moveToState s
    else removeToState s

popState :: MonadState GlobalState m => m ()
popState = do
  s <- use stack
  let top = Stack.head s
      s'  = Stack.pop s
  states %= M.delete top
  stack  .= s'

popStateOrQuit :: EventM n GlobalState ()
popStateOrQuit = 
  do popState
     s <- use stack
     when (Stack.size s == 0) halt

safeGetState :: MonadState GlobalState m => m (Maybe State)
safeGetState = do
  gs <- get
  return $ do 
    key <- safeHead (gs ^. stack)
    M.lookup key (gs ^. states)

goToModeOrQuit :: Mode -> EventM n GlobalState ()
goToModeOrQuit mode = do
  mMode <- M.lookup mode <$> use states
  maybe halt goToState mMode 

removeToMode :: MonadState GlobalState m => Mode -> m ()
removeToMode m = do
  popState
  current <- Stack.head <$> use stack
  if current == m
    then return ()
    else removeToMode m

removeToModeOrQuit :: Mode -> EventM n GlobalState ()
removeToModeOrQuit = removeToModeOrQuit' $ return ()

removeToModeOrQuit' :: EventM n GlobalState () -> Mode -> EventM n GlobalState ()
removeToModeOrQuit' beforeMoving mode = do
  mState <- M.lookup mode <$> use states
  case mState of
    Nothing -> halt
    Just m -> do
      gs <- get
      beforeMoving
      removeToMode mode

refreshRecents :: (MonadState CSS m, MonadIO m) => m ()
refreshRecents = do
  rs <- liftIO getRecents
  let prettyRecents = shortenFilepaths (toList rs)
      options       = Vec.fromList (prettyRecents ++ ["Select file from system"])
  recents .= rs
  list .= L.list Ordinary options 1
