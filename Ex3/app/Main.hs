{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Environment (getArgs, getProgName)

import Lens.Micro.Platform

import Brick (
  AttrMap,
  AttrName,
  BrickEvent (VtyEvent),
  EventM,
  Widget,
  attrMap,
  attrName,
  bg,
  defaultMain,
  fg,
  halt,
  showFirstCursor,
 )
import Brick qualified (App (..))
import Brick.Types (ViewportType (..))
import Brick.Widgets.Border (border)
import Brick.Widgets.Center
import Brick.Widgets.Core
import Graphics.Vty (blue, red, yellow)
import Graphics.Vty qualified
import Graphics.Vty.Input.Events (Event (..), Key (..), Modifier (..))

import Text.Megaparsec (
  ParseErrorBundle (bundleErrors, bundlePosState),
  PosState (pstateSourcePos),
  SourcePos (sourceColumn, sourceLine),
  TraversableStream (reachOffset),
  errorBundlePretty,
  errorOffset,
 )
import Text.Megaparsec.Pos (unPos)

import Cursor (Cursor, Span (..))
import Cursor qualified as C
import Parser qualified as P

data EditMode = Normal | Insert | Command

data AppState = AppState {_filename :: String, _editMode :: EditMode, _editBuffer :: Cursor, _commandContent :: Text}
makeLenses ''AppState

data ResourceName = FileContent | FileCursor deriving (Eq, Ord, Show)

annotationToAttr :: P.Annotation -> AttrName
annotationToAttr P.HighlightedName = attrName "highlightedName"
annotationToAttr P.HighlightedParen = attrName "highlightedParen"
annotationToAttr P.IntLit = attrName "intLit"

appDraw :: AppState -> [Widget ResourceName]
appDraw s = case s ^. editMode of
  Normal -> [fc marks]
  Insert -> [fc (Nothing, [])]
  Command -> [commandBox (s ^. commandContent), fc marks]
 where
  fc (err, hs) = (<=> fromMaybe emptyWidget err) $ viewport FileContent Both $ C.render FileCursor hs $ s ^. editBuffer
  minCmdSize = 10
  toHs :: (P.Span, P.Annotation) -> (Span, AttrName)
  toHs (sp, a) =
    ( Span
        (sp ^. P.spanStart . P.posLine)
        (sp ^. P.spanStart . P.posCol)
        (sp ^. P.spanEnd . P.posLine)
        (sp ^. P.spanEnd . P.posCol)
    , annotationToAttr a
    )
  marks =
    case P.parseExpr (s ^. filename) (s ^. editBuffer . C.toText) of
      (Right expr) -> (Nothing, toHs <$> P.annotations expr (P.fromPair $ s ^. editBuffer . C.pos))
      (Left e) ->
        let errPos = pstateSourcePos $ snd $ reachOffset (errorOffset $ NE.head $ bundleErrors e) (bundlePosState e)
            errLine = unPos $ sourceLine errPos
            errCol = unPos $ sourceColumn errPos
         in ( Just $ border $ padRight Max $ str $ errorBundlePretty e
            , [(Span errLine errCol errLine (errCol + 1), attrName "parseError")]
            )
  commandBox cmd = hCenterLayer $ padTop (Pad 1) $ border $ padRight (Pad $ max (minCmdSize - T.length cmd) 0) $ txt $ T.cons ':' cmd

appHandleEvent :: BrickEvent ResourceName () -> EventM ResourceName AppState ()
appHandleEvent (VtyEvent (EvKey (KChar 'c') [MCtrl])) = halt
appHandleEvent e = do
  m <- use editMode
  case m of
    Normal -> handleEventNormal e
    Insert -> handleEventInsert e
    Command -> handleEventCommand e

handleEventNormal :: BrickEvent ResourceName () -> EventM ResourceName AppState ()
handleEventNormal (VtyEvent (EvKey (KChar ':') [])) = editMode .= Command >> commandContent .= ""
handleEventNormal (VtyEvent (EvKey (KChar 'i') [])) = editMode .= Insert
handleEventNormal (VtyEvent (EvKey (KChar 'j') [])) = editBuffer %= C.moveCursorRowRel 1
handleEventNormal (VtyEvent (EvKey (KChar 'k') [])) = editBuffer %= C.moveCursorRowRel (-1)
handleEventNormal (VtyEvent (EvKey (KChar 'l') [])) = editBuffer %= C.moveCursorColRel 1
handleEventNormal (VtyEvent (EvKey (KChar 'h') [])) = editBuffer %= C.moveCursorColRel (-1)
handleEventNormal (VtyEvent (EvKey (KChar 'G') [])) = editBuffer %= C.moveCursorRowRel (maxBound :: Int)
handleEventNormal (VtyEvent (EvKey (KChar 'g') [])) = editBuffer %= C.moveCursorRowRel (minBound :: Int)
handleEventNormal (VtyEvent (EvKey (KChar 'A') [])) = (editBuffer %= C.moveCursorColRel (maxBound :: Int)) >> (editMode .= Insert)
handleEventNormal _ = return ()

handleEventCommand :: BrickEvent ResourceName () -> EventM ResourceName AppState ()
handleEventCommand (VtyEvent (EvKey KEnter [])) = handleEditorCommand
handleEventCommand (VtyEvent (EvKey KBS [])) = commandContent %= T.dropEnd 1
handleEventCommand (VtyEvent (EvKey (KChar c) [])) = commandContent %= (`T.snoc` c)
handleEventCommand _ = return ()

handleEditorCommand :: EventM ResourceName AppState ()
handleEditorCommand = do
  cmd <- use commandContent
  commandContent .= ""
  editMode .= Normal
  case cmd of
    "q" -> halt
    "quit" -> halt
    "write" -> do
      fn <- use filename
      c <- use editBuffer
      liftIO $ TIO.writeFile fn (c ^. C.toText)
    _ -> return ()

handleEventInsert :: BrickEvent ResourceName () -> EventM ResourceName AppState ()
handleEventInsert (VtyEvent (EvKey (KChar c) [])) = editBuffer %= C.insertChar c
handleEventInsert (VtyEvent (EvKey KEsc [])) = editMode .= Normal
handleEventInsert (VtyEvent (EvKey KBS [])) = editBuffer %= C.deleteChar
handleEventInsert (VtyEvent (EvKey KEnter [])) = editBuffer %= C.insertLB
handleEventInsert _ = return ()

appAttrMap :: AppState -> AttrMap
appAttrMap =
  const $
    attrMap
      Graphics.Vty.defAttr
      [ (attrName "parseError", bg red)
      , (attrName "intLit", fg yellow)
      , (attrName "highlightedName", fg blue)
      , (attrName "highlightedParen", fg blue)
      ]

main :: IO ()
main = do
  file <- listToMaybe <$> getArgs
  case file of
    Nothing -> do
      progName <- getProgName
      putStrLn $ "Usage: " ++ progName ++ " <file>"
    (Just f) -> do
      content <- TIO.readFile f
      let app = Brick.App appDraw showFirstCursor appHandleEvent (return ()) appAttrMap
          initialState = AppState f Normal (C.initCursor content) ""
      _ <- defaultMain app initialState
      return ()
