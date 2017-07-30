module Lexer
  ( Lexer
  , lex
  , Id (..)
  , TokPos (..)
  , Tok (..)
  , lex'
  ) where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad
import Data.Functor
import Data.List (foldl')
import qualified Data.Text as T
import Data.Void
import Prelude hiding (lex)
import Text.Megaparsec
import Text.Megaparsec.Char hiding (satisfy, space)
import qualified Text.Megaparsec.Char.Lexer as L
--------------------------------------------------------------------------------
import Token
--------------------------------------------------------------------------------

lex' :: String -> [TokPos]
lex' s =
    case runParser lex "" s of
      Left err -> error (show err)
      Right ts -> ts

--------------------------------------------------------------------------------

-- Parsing a String is probably faster than Text here, because of allocating
-- Text uncons.
type Lexer = Parsec Void String

space :: Lexer ()
space = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Lexer Tok -> Lexer TokPos
lexeme l = do
    pos <- getPosition
    tok <- L.lexeme space l
    return (TokPos tok pos)

lex :: Lexer [TokPos]
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

lexWildIdent =
    char '_' >> fmap (mkId . ('_' :)) (many alphaNumChar)
