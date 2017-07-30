module Token where

--------------------------------------------------------------------------------
import qualified Data.Text as T
import Text.Megaparsec.Pos (SourcePos)
--------------------------------------------------------------------------------

newtype Id = Id { idText :: T.Text }
  deriving (Eq, Ord)

instance Show Id where
  show = T.unpack . idText

-- instance Show Id where
--   show = ('\'' :) . T.unpack . idText

mkId :: String -> Id
mkId = Id . T.pack

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

data TokPos = TokPos
  { _tok :: Tok
  , _pos :: SourcePos
  } deriving (Show, Eq, Ord)

tokLen :: Tok -> Int
tokLen = undefined
