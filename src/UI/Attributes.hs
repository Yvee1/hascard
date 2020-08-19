module UI.Attributes where
import Brick
import Brick.Forms
import Graphics.Vty

titleAttr :: AttrName
titleAttr = attrName "title"

selectedAttr :: AttrName
selectedAttr = attrName "selected"

textboxAttr :: AttrName
textboxAttr = attrName "textbox"

highlightedChoiceAttr :: AttrName
highlightedChoiceAttr = attrName "highlighted choice"

incorrectChoiceAttr :: AttrName
incorrectChoiceAttr = attrName "incorrect choice"

correctChoiceAttr :: AttrName
correctChoiceAttr = attrName "correct choice"

highlightedOptAttr :: AttrName
highlightedOptAttr = attrName "highlighted option"

selectedOptAttr :: AttrName
selectedOptAttr = attrName "selected option"

correctOptAttr :: AttrName
correctOptAttr = attrName "correct option"

incorrectOptAttr :: AttrName
incorrectOptAttr = attrName "incorrect option"

gapAttr :: AttrName
gapAttr = attrName "gap"

incorrectGapAttr :: AttrName
incorrectGapAttr = attrName "incorrect gap"

correctGapAttr :: AttrName
correctGapAttr = attrName "correct gap"

highlightedElementAttr :: AttrName
highlightedElementAttr = attrName "highlighted element"

grabbedElementAttr :: AttrName
grabbedElementAttr = attrName "grabbed element"

correctElementAttr :: AttrName
correctElementAttr = attrName "correct element"

incorrectElementAttr :: AttrName
incorrectElementAttr = attrName "incorrect element"

exceptionAttr :: AttrName
exceptionAttr = attrName "exception"

shuffledAttr :: AttrName
shuffledAttr = attrName "shuffled indicator"

yesButtonAttr :: AttrName
yesButtonAttr = attrName "yes button"

noButtonAttr :: AttrName
noButtonAttr = attrName "no button"

selectedYesButtonAttr :: AttrName
selectedYesButtonAttr = attrName "selected yes button"

selectedNoButtonAttr :: AttrName
selectedNoButtonAttr = attrName "selected no button"

correctAttr :: AttrName
correctAttr = attrName "correct"

wrongAttr :: AttrName
wrongAttr = attrName "incorrect"

theMap :: AttrMap
theMap = attrMap defAttr
  [ (titleAttr, fg yellow)
  , (textboxAttr, defAttr)
  , (highlightedChoiceAttr, fg yellow)
  , (incorrectChoiceAttr, fg red)
  , (correctChoiceAttr, fg green)
  , (incorrectGapAttr, fg red `withStyle` underline)
  , (correctGapAttr, fg green `withStyle` underline)
  , (highlightedOptAttr, fg yellow)
  , (selectedOptAttr, fg blue)
  , (incorrectOptAttr, fg red)
  , (correctOptAttr, fg green)
  , (highlightedElementAttr, fg yellow)
  , (grabbedElementAttr, fg blue)
  , (correctElementAttr, fg green)
  , (incorrectElementAttr, fg red)
  , (gapAttr, defAttr `withStyle` underline)
  , (selectedAttr, defAttr `withStyle` underline)
  , (exceptionAttr, fg red)
  , (focusedFormInputAttr, defAttr `withStyle` underline)
  , (shuffledAttr, fg red)
  , (correctAttr, fg green)
  , (wrongAttr, fg red)
  , (yesButtonAttr, fg green)
  , (noButtonAttr, fg red)
  , (selectedYesButtonAttr, fg green `withStyle` underline)
  , (selectedNoButtonAttr, fg red `withStyle` underline)
  ]