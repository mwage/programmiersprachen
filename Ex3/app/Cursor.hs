{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Cursor (
  initCursor,
  toText,
  Cursor (Cursor),
  pos,
  prev,
  next,
  render,
  moveCursorRel,
  moveCursorColRel,
  moveCursorRowRel,
  Span (..),
  currentChar,
  insertChar,
  deleteChar,
  insertLB,
) where

import Data.Foldable (foldl')
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

import Lens.Micro.Platform

import Brick (Location (Location), Widget, attrName, hBox, showCursor, txt, vBox, visible)
import Brick.AttrMap (AttrName)
import Brick.Widgets.Core (withAttr)

-- cursor pos: 1 based
data Cursor = Cursor {_prev :: [Text], _next :: [Text], _pos :: (Int, Int)} deriving (Eq, Show)
makeLenses ''Cursor

-- 1-based; Lines: start incl. end incl. Col: start incl. end excl.
data Span = Span {_spStartRow :: Int, _spStartCol :: Int, _spEndRow :: Int, _spEndCol :: Int} deriving (Show, Eq)
makeLenses ''Span

pointToSpan :: (Int, Int) -> Span
pointToSpan (x, y) = Span x y x (y + 1)

initCursor :: Text -> Cursor
initCursor t = Cursor [] (T.lines $ T.replace "\r" "" $ T.replace "\t" "  " t) (1, 1)

toText :: SimpleGetter Cursor Text
toText = to (\(Cursor{_prev, _next}) -> T.unlines $ reverse _prev ++ _next)

currentChar :: Cursor -> Maybe Char
currentChar = preview $ next . _head . _head

insertChar :: Char -> Cursor -> Cursor
insertChar c (Cursor p [] cpos) = Cursor p [T.singleton c] (cpos & _2 %~ (+ 1))
insertChar c (Cursor p (n : ns) cpos) = Cursor p (updatedLine : ns) (cpos & _2 %~ (+ 1))
 where
  updatedLine = let (beg, end) = T.splitAt (snd cpos - 1) n in beg `T.append` T.cons c end

deleteChar :: Cursor -> Cursor
deleteChar cur@(Cursor p ns cpos)
  | snd cpos == 1 = case p of
      [] -> cur
      (p' : ps) -> Cursor ps (T.append p' (ns ^. _head) : (ns ^. _tail)) (fst cpos - 1, T.length p' + 1)
  | otherwise =
      let
        updatedLine = let (beg, end) = T.splitAt (snd cpos - 1) (ns ^. _head) in T.dropEnd 1 beg `T.append` end
       in
        Cursor p (updatedLine : ns ^. _tail) (cpos & _2 %~ (\x -> max 1 $ x - 1))

insertLB :: Cursor -> Cursor
insertLB (Cursor p [] cpos) = Cursor p [""] cpos
insertLB (Cursor p (n : ns) cpos) = Cursor (h : p) (t : ns) ((cpos ^. _1) + 1, 1)
 where
  (h, t) = T.splitAt (snd cpos - 1) n

renderLine :: n -> [((Int, Int), [AttrName])] -> Text -> Widget n
renderLine cn highlights content = hBox $ go 1 highlights (if T.length content == 0 then " " else content)
 where
  go _ [] t = [txt t]
  go i hs@(((s, e), as) : hs') t
    | i < s = let (x, y) = T.splitAt (s - i) t in txt x : go s hs y
    | i == s =
        let (x, y) = T.splitAt ((e + 1) - i) t
            widget = foldl' (flip withAttr) (txt x) as
         in ( if attrName "cursor" `elem` as
                then visible $ showCursor cn (Location (0, 0)) widget
                else widget
            )
              : go (e + 1) hs' y
    | otherwise = error "foo"

-- ensures that all spans are non overlapping and dont span multiple lines
prepareAttrMap :: [(Span, AttrName)] -> Map Int [((Int, Int), [AttrName])]
prepareAttrMap spans = prepareLine . sortSpans <$> splitLines
 where
  splitLines =
    foldl'
      ( \m (s, a) ->
          foldl'
            (\m' i -> M.insertWith (++) i [((s ^. spStartCol, s ^. spEndCol - 1), [a])] m')
            m
            [s ^. spStartRow .. s ^. spEndRow]
      )
      M.empty
      spans
  sortSpans = sortOn (^. _1 . _1)
  prepareLine :: [((Int, Int), [AttrName])] -> [((Int, Int), [AttrName])]
  prepareLine [] = []
  prepareLine x@[_] = x
  prepareLine (x@(s1, a) : y@(s2, b) : xs)
    | fst s1 == fst s2 =
        if snd s1 == snd s2
          then prepareLine ((s1, a ++ b) : xs)
          else
            let minEnd = min (snd s1) (snd s2)
                maxEnd = max (snd s1) (snd s2)
                attr = if snd s1 < snd s2 then b else a
             in prepareLine $ ((fst s1, minEnd), a ++ b) : sortSpans (((minEnd + 1, maxEnd), attr) : xs)
    | snd s1 < fst s2 = x : prepareLine (y : xs)
    -- overlapping
    | otherwise = ((fst s1, fst s2 - 1), a) : prepareLine ((s1 & _1 .~ fst s2, a) : y : xs)

render :: n -> [(Span, AttrName)] -> Cursor -> Widget n
render cn sps c = vBox $ zipWith (\l t -> renderLine cn (fromMaybe [] $ amap M.!? l) t) [1 ..] (reverse (c ^. prev) ++ (c ^. next) ++ [" "])
 where
  amap = prepareAttrMap ((pointToSpan $ c ^. pos, attrName "cursor") : sps)

moveCursorRel :: (Int, Int) -> Cursor -> Cursor
moveCursorRel (0, 0) c = c
moveCursorRel (row, col) c = moveCursorRowRel row $ moveCursorColRel col c

fixCol :: Cursor -> Cursor
fixCol c = c & (pos . _2) %~ (max 1 . min (lineLen + 1))
 where
  lineLen = maybe 0 T.length $ c ^? (next . _head)

moveCursorColRel :: Int -> Cursor -> Cursor
moveCursorColRel 0 c = c
moveCursorColRel o c
  | o == maxBound = fixCol (c & (pos . _2) .~ maxBound)
  | o == minBound = c & (pos . _2) .~ 1
  | otherwise = fixCol (c & (pos . _2) %~ (+ o))

moveCursorRowRel :: Int -> Cursor -> Cursor
moveCursorRowRel 0 c = c
moveCursorRowRel o c
  | o == 0 = c
  | o > 0 =
      let
        (p, n, r) = advance o (c ^. prev) (c ^. next)
       in
        fixCol ((c{_prev = p, _next = n}) & (pos . _1) %~ (+ (o - r)))
  | otherwise =
      let
        (p, n, r) = back (abs o) (c ^. prev) (c ^. next)
       in
        fixCol ((c{_prev = p, _next = n}) & (pos . _1) %~ subtract (abs o - r))

advance :: Int -> [a] -> [a] -> ([a], [a], Int)
advance n x [] = (x, [], n)
advance 0 x y = (x, y, 0)
advance n xs (y : ys) = advance (n - 1) (y : xs) ys

back :: Int -> [a] -> [a] -> ([a], [a], Int)
back n [] y = ([], y, n)
back 0 x y = (x, y, 0)
back n (x : xs) ys = back (n - 1) xs (x : ys)
