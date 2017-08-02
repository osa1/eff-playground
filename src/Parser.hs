{-# OPTIONS_GHC -fno-warn-orphans #-}

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
import Data.Semigroup
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Void
import Prelude hiding (lex)
import Safe (lastMay)
import Text.Megaparsec hiding (sepBy1)
import Text.Show.Pretty (ppShow)
--------------------------------------------------------------------------------
import AST
import Lexer (lex')
import Token
--------------------------------------------------------------------------------

parse' :: Parser a -> String -> a
parse' p s =
    case runParser p "" (lex' s) of
      Left err -> error (show err)
      Right a  -> a

parsePrint :: Show a => Parser a -> String -> IO ()
parsePrint p s = putStrLn (ppShow (parse' p s))

--------------------------------------------------------------------------------

type Parser = Parsec Void [TokPos]

instance Stream [TokPos] where
  type Token [TokPos] = TokPos
  type Tokens [TokPos] = [TokPos]
  tokenToChunk _ = pure
  tokensToChunk _ = id
  chunkToTokens _ = id
  chunkLength _ = length
  chunkEmpty _ = null

  advance1 _ _ _ (TokPos tok_ pos) =
    pos{ sourceColumn = sourceColumn pos <> mkPos (tokLen tok_) }

  advanceN _ _ pos0 toks =
    case lastMay toks of
      Nothing -> pos0
      Just (TokPos tok_ pos) ->
        pos{ sourceColumn = sourceColumn pos <> mkPos (tokLen tok_) }

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
      case f (_tok x) of
        Nothing -> Left (pure (Tokens (x:|[])), S.empty)
        Just x' -> Right x'

tok :: Tok -> Parser ()
tok t = void (token testChar Nothing)
  where
    testChar x =
      if t == _tok x
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

parseDataDecl :: Parser DataD
parseDataDecl = do
    tok Data
    n <- satisfy (\case ConIdentTok t -> Just t; _ -> Nothing)
    ty_args <- many (satisfy (\case IdentTok t -> Just t; _ -> Nothing))
    tok Equals
    cons <- con `sepBy1` tok Bar
    return (DataD n ty_args cons)
  where
    con = do
      con_id <- satisfy (\case ConIdentTok t -> Just t; _ -> Nothing)
      tok Colon
      args <- valTy `sepBy` tok Arrow
      return (Con con_id args)

parseValDecl :: Parser (ValD ValTy Expr)
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
    , SuspendE <$> comp
    , between (tok LParen) (tok RParen)
              (sepBy1 parseExpr (tok Comma) >>= \case
                e :| [] -> return e
                e :| es -> return (TupE (e : es)))

    , let_
    -- TODO: letrec
    ]

let_ :: Parser Expr
let_ = do
    tok Let
    i <- satisfy (\case IdentTok t -> Just t; _ -> Nothing)
    tok Colon
    ty <- valTy
    tok Equals
    e1 <- parseExpr
    tok In
    e2 <- parseExpr
    return (LetE i ty e1 e2)

comp :: Parser Comp
comp = between (tok LBrace) (tok RBrace) (comp1 `sepBy1` tok Bar)

comp1 :: Parser ([CompPat], Expr)
comp1 = do
    ps <- some pat
    tok Arrow
    e <- parseExpr
    return (ps, e)

pat :: Parser CompPat
pat = msum
    [ between (tok LParen) (tok RParen) pat
    , ValPat <$> valPat
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

unsnoc :: [a] -> Maybe ([a], a)
unsnoc []  = Nothing
unsnoc [x] = Just ([], x)
unsnoc (x : xs) = first (x:) <$> unsnoc xs
