{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Parser where

import Control.Applicative (Alternative (..), some)

import Data.Char (isAsciiLower, isAsciiUpper)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)

import Control.Monad (when)
import Control.Monad.State.Strict (State, evalState)
import Data.Foldable (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Lens.Micro.Platform
import Text.Megaparsec (MonadParsec (..), ParseErrorBundle, Parsec, SourcePos (..), TraversableStream, errorBundlePretty, getSourcePos, parse, satisfy, unPos, (<?>))
import Text.Megaparsec.Char (char, digitChar, space, space1)
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Stream (Token)

type Parser = Parsec Void Text

data Pos = Pos {_posLine :: Int, _posCol :: Int} deriving (Show, Eq)
makeLenses ''Pos

instance Ord Pos where
  (Pos sr sc) <= (Pos er ec) = sr < er || (sr == er && sc <= ec)

fromPair :: (Int, Int) -> Pos
fromPair (r, c) = Pos r c

-- 1 based; Line: start incl.; end incl; Col: start incl. end excl..
data Span = Span
  { _spanStart :: Pos
  , _spanEnd :: Pos
  , _spanSub :: [Span] -- sub spans for more complex terms e.g. name in a lambda abstraction
  , _spanParens :: [(Pos, Pos)]
  }
  deriving (Show, Eq)
makeLenses ''Span

emptySpan :: Span
emptySpan = Span (Pos 1 1) (Pos 1 2) [] []

fromPos :: Pos -> Span
fromPos p = Span p (p & posCol %~ (+ 1)) [] []

contains :: Span -> Pos -> Bool
contains (Span s e _ _) p = s <= p && p < e

data Expr m
  = Lambda {_exprMeta :: m, _lambdaParam :: Text, _lambdaExpr :: Expr m}
  | App {_exprMeta :: m, _appArgs :: [Expr m]}
  | I {_exprMeta :: m, _iVal :: Int}
  | Name {_exprMeta :: m, _nameVal :: Text}
  | LazyRecord {_exprMeta :: m, _recFields :: [(Text, Expr m)]}
  | EagerRecord {_exprMeta :: m, _recFields :: [(Text, Expr m)]}
  deriving (Show, Eq)
makeLenses ''Expr

type LocExpr = Expr Span

newtype Context = Context {_ctxDecls :: Map Text Span} deriving (Show, Eq)
makeLenses ''Context

data Annotation = HighlightedName | HighlightedParen | IntLit deriving (Show, Eq)
data AnnotationState = AnnotationState
  { _stPos :: Pos
  , _stNames :: Map Text (Map Int [Span])
  , _stName :: Maybe (Text, Int)
  , _stAnnos :: [(Span, Annotation)]
  }
  deriving (Show)
makeLenses ''AnnotationState

checkHighlight :: Text -> Span -> State AnnotationState ()
checkHighlight n s = do
  p <- use stPos
  when (s `contains` p) $ do
    names <- use stNames
    stName .= Just (n, maybe 1 fst $ M.lookupMax $ names M.! n)

addName :: Text -> Span -> Bool -> State AnnotationState ()
addName n s new = (stNames %= \names -> M.alter alterF n names) >> checkHighlight n s
 where
  alterF Nothing = Just $ M.singleton 1 [s]
  alterF (Just ns) = Just $ M.insertWith (++) (maybe 1 ((if new then (+ 1) else id) . fst) $ M.lookupMax ns) [s] ns

highlightParens :: LocExpr -> State AnnotationState ()
highlightParens expr = do
  p <- use stPos
  stAnnos %= \x -> concatMap (\(o, c) -> [(fromPos o, HighlightedParen), (fromPos c, HighlightedParen)]) (filter (\(o, c) -> p == o || p == c) (expr ^. exprMeta . spanParens)) ++ x
  case expr of
    (LazyRecord (Span s e _ _) _) | p == s || p == e -> stAnnos %= \x -> (fromPos s, HighlightedParen) : (fromPos e, HighlightedParen) : x
    (EagerRecord (Span s e _ _) _) | p == s || p == e -> stAnnos %= \x -> (fromPos s, HighlightedParen) : (fromPos e, HighlightedParen) : x
    _ -> return ()

highlightName :: (Text, Int) -> State AnnotationState ()
highlightName (n, i) = do
  names <- use stNames
  stAnnos %= (\x -> ((,HighlightedName) <$> fromMaybe [] (M.lookup n names >>= M.lookup i)) ++ x)
  return ()

addAnno :: Span -> Annotation -> State AnnotationState ()
addAnno s a = stAnnos %= ((s, a) :)

finalizeState :: State AnnotationState ()
finalizeState = do
  mhn <- use stName
  case mhn of
    (Just n) -> highlightName n
    _ -> return ()

filterNames :: Map Text Int -> State AnnotationState ()
filterNames lvls = do
  stNames %= M.mapWithKey (\n m -> M.filterWithKey (\k _ -> maybe False (k <=) $ M.lookup n lvls) m)
  mn <- use stName
  case mn of
    (Just (n, l)) | maybe True (l >) $ M.lookup n lvls -> stName .= Nothing
    _ -> return ()

recordDefs :: LocExpr -> [Text]
recordDefs e = e ^.. (recFields . each . _1)

annotations :: LocExpr -> Pos -> [(Span, Annotation)]
annotations expr pos = evalState (go expr >> use stAnnos) $ AnnotationState pos M.empty Nothing []
 where
  go :: LocExpr -> State AnnotationState () -- [(Span, Annotation)]
  go e =
    highlightParens e >> case e of
      (I s _) -> addAnno s IntLit >> finalizeState
      (Name s t) -> addName t s False >> finalizeState
      (Lambda (Span _ _ (nameSpan : _) _) n se) -> addName n nameSpan True >> go se
      (LazyRecord (Span _ _ sps _) ses) -> handleRec $ zipWith (\s (n, se) -> (n, s, se)) (concatMap (^. spanSub) sps) ses
      (EagerRecord (Span _ _ sps _) ses) -> handleRec $ zipWith (\s (n, se) -> (n, s, se)) (concatMap (^. spanSub) sps) ses
      (App _ ses) -> do
        names <- use stNames
        let lvls = maybe 0 fst . M.lookupMax <$> names
        case ses of
          [r@(LazyRecord _ _), n@(Name _ _)] -> go r >> go n >> filterNames lvls >> finalizeState
          _ -> mapM_ (\x -> go x >> filterNames lvls) ses >> finalizeState
      _ -> error "Invalid AST"
   where
    handleRec :: [(Text, Span, LocExpr)] -> State AnnotationState ()
    handleRec ses = do
      names <- use stNames
      let lvls = maybe 0 fst . M.lookupMax <$> names
      f lvls ses
     where
      f :: Map Text Int -> [(Text, Span, LocExpr)] -> State AnnotationState ()
      f _ [] = return ()
      f lvls ((n, sp, se) : ses') = do
        go se
        addName n sp True
        names <- use stNames
        let lvls' = (flip $ M.insert n) lvls $ fst $ M.findMax $ names M.! n
        filterNames lvls'
        f lvls' ses'

getPos :: (TraversableStream s, MonadParsec e s m) => m Pos
getPos = getSourcePos <&> \(SourcePos _ l c) -> Pos (unPos l) (unPos c)

withSpan :: (TraversableStream s, MonadParsec e s m) => m (Expr Span) -> m LocExpr
withSpan p = do
  start <- getPos
  e <- p
  end <- getPos
  return (e & exprMeta .~ Span start end [] [])

withSpan' :: (TraversableStream s, MonadParsec e s m) => m a -> m (Span, a)
withSpan' p = do
  start <- getPos
  e <- p
  end <- getPos
  return (Span start end [] [], e)

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

paren :: Parser LocExpr -> Parser LocExpr
paren p = do
  open <- getPos
  _ <- lexeme (char '(')
  e <- p
  close <- getPos
  _ <- lexeme (char ')')
  return (e & (exprMeta . spanParens) %~ ((open, close) :))

asciiLetter :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
asciiLetter = satisfy (\c -> isAsciiLower c || isAsciiUpper c)

asciiAlnum :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
asciiAlnum = asciiLetter <|> digitChar

identifier :: Parser Text
identifier = T.pack <$> ((:) <$> (asciiLetter <|> char '_') <*> many (asciiAlnum <|> char '_'))

int :: Parser Int
int = read <$> some digitChar

name :: Parser LocExpr
name = withSpan' (identifier <?> "name") >>= \(sp, n) -> return $ Name sp n

integer :: Parser LocExpr
integer = withSpan' (int <?> "integer") >>= \(sp, n) -> return $ I sp n

lambda :: Parser LocExpr
lambda = label "lambda abstraction" $ do
  start <- getPos
  (Name nameSp ident) <- lexeme name
  (dotSpan, _) <- withSpan' $ lexeme $ char '.'
  e <- expression
  end <- getPos
  return $ Lambda (Span start end [nameSp, dotSpan, e ^. exprMeta] []) ident e

app :: Parser LocExpr
app = do
  r <- withSpan (App emptySpan <$> some basic <?> "application")
  case r of
    (App _ l) | length l == 1 -> return $ head l
    x -> return x
 where
  basic = lexeme integer <|> lexeme name <|> paren expression <|> record

record :: Parser LocExpr
record = lazyRecord <|> eagerRecord

lazyRecord :: Parser LocExpr
lazyRecord = do
  s <- getPos
  _ <- lexeme (char '{')
  (spans, fields) <- pairs <|> pure ([], [])
  e <- getPos
  _ <- lexeme (char '}')
  return $ LazyRecord (Span s e spans []) fields

eagerRecord :: Parser LocExpr
eagerRecord = do
  s <- getPos
  _ <- lexeme (char '[')
  (spans, fields) <- pairs <|> pure ([], [])
  e <- getPos
  _ <- lexeme (char ']')
  return $ LazyRecord (Span s e spans []) fields

pairs :: Parser ([Span], [(Text, LocExpr)])
pairs = unzip <$> fields
 where
  fields = (:) <$> pair <*> many (lexeme (char ',') >> pair)

pair :: Parser (Span, (Text, LocExpr))
pair = do
  s <- getPos
  (Name sp n) <- lexeme name
  _ <- lexeme $ char '='
  expr <- expression
  e <- getPos
  return (Span s e [sp] [], (n, expr))

expression :: Parser LocExpr
expression =
  try lambda
    <|> app
    <?> "expression"

parseExpr :: FilePath -> Text -> Either (ParseErrorBundle Text Void) LocExpr
parseExpr = parse (space *> expression <* eof)

toExpr :: LocExpr -> Expr ()
toExpr (Lambda _ n e) = Lambda () n $ toExpr e
toExpr (App _ es) = App () $ toExpr <$> es
toExpr (LazyRecord _ fs) = LazyRecord () $ (fmap . fmap) toExpr fs
toExpr (EagerRecord _ fs) = EagerRecord () $ (fmap . fmap) toExpr fs
toExpr (I _ i) = I () i
toExpr (Name _ n) = Name () n
