{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Parser where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad
import Data.Bifunctor (first)
import Data.Functor
import Data.List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Void
import Prelude hiding (lex)
import Text.Megaparsec hiding (sepBy1)
import Text.Megaparsec.Char hiding (satisfy, space)
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Show.Pretty (ppShow)
--------------------------------------------------------------------------------

newtype Id = Id { idText :: T.Text }
  deriving (Eq, Ord)

instance Show Id where
  show = T.unpack . idText

-- instance Show Id where
--   show = ('\'' :) . T.unpack . idText

mkId :: String -> Id
mkId = Id . T.pack

-- | Value types
data ValTy
  -- | Constructor application
  = DataTy Id [TyArg]
  -- | Computation type
  | CompTy_ CompTy
  -- | Type variable
  | VarTy Id
  deriving (Show)

-- | Computation types
data CompTy = CompTy
  { compPorts :: [Port]
  , compPeg   :: Peg
  } deriving (Show)

data TyArg
  = ValTyArg ValTy
  | AbilityTyArg [(Interface, [TyArg])]
  deriving (Show)

data Port = Port
  { portAdjust :: [(Interface, [TyArg])]
  , portType   :: ValTy
  } deriving (Show)

data Peg = Peg
  { pegAbility :: [(Interface, [TyArg])]
  , pegType    :: ValTy
  } deriving (Show)

type Interface = Id

--------------------------------------------------------------------------------

data Decl
  = ValDecl ValD
  | IfaceDecl IfaceD
  deriving (Show)

data ValD = ValD
  { valId        :: Id
  , valDeclType  :: ValTy
  , valDeclValue :: Expr
  } deriving (Show)

data IfaceD = IfaceD
  { ifaceId      :: Id
  , ifaceTyBndrs :: [Id]
  , ifaceCons    :: NonEmpty Con
  } deriving (Show)

data Con = Con
  { conId   :: Id
  , conArgs :: [ValTy]
  } deriving (Show)

--------------------------------------------------------------------------------

data Expr
  = VarE Id
  | AppE Expr [Expr]
  | ConE Id
  | SuspendE Comp
  | LetE Id Expr Expr
  | LetRecE [(Id, Comp)] Comp
  | IntE Int
  | TupE [Expr]
  deriving (Show)

type Comp = NonEmpty ([CompPat], Expr)

data CompPat
  = ValPat ValPat
  | ReqPat Id [ValPat] Id -- cont
  | WildCompPat Id
  deriving (Show)

data ValPat
  = ConPat Id [ValPat]
  | WildValPat Id
  deriving (Show)

--------------------------------------------------------------------------------

data Tok
  = IdentTok Id
  | ConIdentTok Id
  | WildId Id
  | StringTok T.Text
  | IntTok Int
  | Unit
  | Comma
  | Bang
  | Arrow
  | Equals
  | Bar
  | LBrace
  | RBrace
  | LBracket
  | RBracket
  | LAngle
  | RAngle
  | Colon
  | LParen
  | RParen
  -- Keywords
  | Interface
  | Let
  | LetRec
  deriving (Show, Eq, Ord)

-- Parsing a String is probably faster than Text here, because of allocating
-- Text uncons.
type Lexer = Parsec Void String

space :: Lexer ()
space = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Lexer a -> Lexer a
lexeme = L.lexeme space

lex :: Lexer [Tok]
lex = do
  ret <- some $ msum $ map lexeme
    [ string "interface" $> Interface
    , string "let" $> Let
    , string "letrec" $> LetRec
    , string "()" $> Unit
    , IdentTok <$> lexIdent
    , ConIdentTok <$> lexConIdent
    , WildId <$> lexWildIdent
    , StringTok . T.pack <$> (char '"' >> manyTill L.charLiteral (char '"'))
    , IntTok . foldl' (\n c -> n * 10 + c) 0 <$> some L.decimal
    , char ',' $> Comma
    , char '!' $> Bang
    , string "->" $> Arrow
    , char '=' $> Equals
    , char '|' $> Bar
    , char '{' $> LBrace
    , char '}' $> RBrace
    , char '[' $> LBracket
    , char ']' $> RBracket
    , char '<' $> LAngle
    , char '>' $> RAngle
    , char ':' $> Colon
    , char '(' $> LParen
    , char ')' $> RParen
    ]
  eof
  return ret

lexIdent, lexConIdent, lexWildIdent :: Lexer Id

lexIdent = do
    c0 <- lowerChar
    cs <- many alphaNumChar
    return (mkId (c0 : cs))

lexConIdent = do
    c0 <- upperChar
    cs <- many alphaNumChar
    return (mkId (c0 : cs))

lexWildIdent = char '_' >> fmap (mkId . ('_' :)) (many alphaNumChar)

--------------------------------------------------------------------------------

type Parser = Parsec Void [Tok]

instance Stream [Tok] where
  type Token [Tok] = Tok
  type Tokens [Tok] = [Tok]
  tokenToChunk _ = pure
  tokensToChunk _ = id
  chunkToTokens _ = id
  chunkLength _ = length
  chunkEmpty _ = null
  advance1 _ _ p _ = p
  advanceN _ _ p _ = p
  take1_ [] = Nothing
  take1_ (t:ts) = Just (t, ts)
  takeN_ n s
    | n <= 0    = Just ([], s)
    | null s    = Nothing
    | otherwise = Just (splitAt n s)
  takeWhile_ = span

satisfy :: (Tok -> Maybe a) -> Parser a
satisfy f = token testChar Nothing
  where
    testChar x =
      case f x of
        Nothing -> Left (pure (Tokens (x:|[])), S.empty)
        Just x' -> Right x'

tok :: Tok -> Parser ()
tok t = void (token testChar Nothing)
  where
    testChar x =
      if t == x
        then Right ()
        else Left (pure (Tokens (x:|[])), S.empty)

parseIfaceDecl :: Parser IfaceD
parseIfaceDecl = do
    tok Interface
    iface <- satisfy (\case ConIdentTok t -> Just t; _ -> Nothing)
    ty_args <- many (satisfy (\case IdentTok t -> Just t; _ -> Nothing))
    tok Equals
    cons <- con `sepBy1` tok Bar
    return (IfaceD iface ty_args cons)
  where
    con = do
      con_id <- satisfy (\case ConIdentTok t -> Just t; _ -> Nothing)
      tok Colon
      args <- valTy `sepBy` tok Arrow
      return (Con con_id args)

parseValDecl :: Parser ValD
parseValDecl = do
    n <- satisfy (\case IdentTok t -> Just t; _ -> Nothing)
    tok Colon
    ty <- valTy
    tok Equals
    e <- parseExpr
    return (ValD n ty e)

valTy, dataTy :: Parser ValTy
valTy = msum
    [ dataTy
    , tok Unit >> return (DataTy (Id "()") [])
    , satisfy (\case IdentTok t -> Just (VarTy t); _ -> Nothing)
    , CompTy_ <$> compType
    ]

dataTy = do
    con <- satisfy (\case ConIdentTok t -> Just t; _ -> Nothing)
    args <- many tyArg
    return (DataTy con args)

tyArg :: Parser TyArg
tyArg = ValTyArg <$> valTy -- TODO AbilityTyArg

{-
let Right toks = parse lex "asdf" "interface State s = Get : s | Put : s -> ()" in parse parseIfaceDecl "adsf" toks
-}

parseExpr :: Parser Expr
parseExpr = do
    es <- some exprNotApp
    return $ case es of
      []  -> error "unreachable"
      [e] -> e
      e : es' -> AppE e es'

exprNotApp :: Parser Expr
exprNotApp = msum
    [ tok Unit $> AppE (VarE (Id "()")) []
    , satisfy (\case IntTok i -> Just (IntE i); _ -> Nothing)
    , satisfy (\case IdentTok t -> Just (VarE t); _ -> Nothing)
    , satisfy (\case ConIdentTok t -> Just (ConE t); _ -> Nothing)
    , SuspendE <$>
      between (tok LBrace) (tok RBrace)
              (comp `sepBy1` tok Bar)
    , between (tok LParen) (tok RParen)
              (sepBy1 parseExpr (tok Comma) >>= \case
                e :| [] -> return e
                e :| es -> return (TupE (e : es)))

    -- TODO: let
    -- TODO: letrec
    ]

comp :: Parser ([CompPat], Expr)
comp = do
    ps <- some pat
    tok Arrow
    e <- parseExpr
    return (ps, e)

pat :: Parser CompPat
pat = msum
    [ ValPat <$> valPat
    , between (tok LAngle)
              (tok RAngle)
              (msum [ satisfy (\case IdentTok t -> Just (WildCompPat t); _ -> Nothing)
                    , do req <- satisfy (\case ConIdentTok t -> Just t; _ -> Nothing)
                         bndrs <- many valPat
                         tok Arrow
                         k <- satisfy (\case IdentTok t -> Just t; _ -> Nothing)
                         return (ReqPat req bndrs k)
                    ])
    ]

valPat :: Parser ValPat
valPat = msum
    [ do con <- satisfy (\case ConIdentTok t -> Just t; _ -> Nothing)
         args <- many valPat
         return (ConPat con args)

    , satisfy (\case IdentTok t -> Just (WildValPat t)
                     WildId i -> Just (WildValPat i)
                     _ -> Nothing)
    ]

--------------------------------------------------------------------------------

parseValType :: Parser ValTy
parseValType = msum
    [ tupleTy, dataType, suspendType, typeVar ]

tupleTy :: Parser ValTy
tupleTy = do
    tys <- between (tok LParen) (tok RParen)
             (tyArg `sepBy` tok Comma)
    return (DataTy (Id (T.pack ('(' : replicate (length tys) ',' ++ ")"))) tys)

{-
listTy :: Parser ValTy
listTy = do
    ty <- between (tok LBracket) (tok RBracket) tyArg
    return (DataTy (Id "[]") [ty])
-}

dataType :: Parser ValTy
dataType = do
    con <- satisfy (\case ConIdentTok t -> Just t; _ -> Nothing)
    args <- many tyArg
    return (DataTy con args)

suspendType :: Parser ValTy
suspendType =
    CompTy_ <$>
    between (tok LBrace) (tok RBrace)
            compType

typeVar :: Parser ValTy
typeVar = satisfy (\case IdentTok t -> Just (VarTy t); _ -> Nothing)

compType :: Parser CompTy
compType = do
    ports0 <- portTy `sepBy` try (tok Arrow)
    mb_peg <- optional $ do
      tok Arrow
      pegTy
    case mb_peg of
      Nothing ->
        case unsnoc ports0 of
          Nothing -> fail "Empty comp type"
          Just (ports, peg) ->
            case portToPeg peg of
              Nothing -> fail "Comp type without peg"
              Just peg' -> return (CompTy ports peg')
      Just peg ->
        return (CompTy ports0 peg)

portToPeg :: Port -> Maybe Peg
portToPeg (Port [] ty) = Just (Peg [] ty)
portToPeg _ = Nothing

portTy :: Parser Port
portTy = do
    adjust <- parseAdjust
    ty <- parseValType
    return (Port adjust ty)

parseAdjust :: Parser [(Interface, [TyArg])]
parseAdjust = do
    mb_adjust <-
      optional $ between
        (tok LAngle) (tok RAngle)
        (parseAdjustContent `sepBy` tok Comma)
    return (fromMaybe [] mb_adjust)

parseAdjustContent :: Parser (Interface, [TyArg])
parseAdjustContent = do
    iface <- satisfy (\case ConIdentTok t -> Just t; _ -> Nothing)
    args <- many tyArg
    return (iface, args)

pegTy :: Parser Peg
pegTy = do
    mb_ability <-
      optional $ between
        (tok LBracket) (tok RBracket)
        (parseAdjustContent `sepBy` tok Comma)
    ty <- parseValType
    return (Peg (fromMaybe [] mb_ability) ty)

--------------------------------------------------------------------------------

sepBy1 :: Alternative m => m a -> m sep -> m (NonEmpty a)
sepBy1 p sep = (:|) <$> p <*> many (sep *> p)

parse' :: (Show e, Show (Token s), Show a) => Parsec e s a -> s -> IO ()
parse' p s = case parse p "" s of
               Left err -> error ("parse failed: " ++ show err)
               Right a -> putStrLn (ppShow a)

unsnoc :: [a] -> Maybe ([a], a)
unsnoc []  = Nothing
unsnoc [x] = Just ([], x)
unsnoc (x : xs) = first (x:) <$> unsnoc xs
