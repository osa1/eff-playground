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
  | Data
  | Let
  | LetRec
  | In
  deriving (Show, Eq, Ord)

data TokPos = TokPos
  { _tok :: Tok
  , _pos :: SourcePos
  } deriving (Show, Eq, Ord)

tokLen :: Tok -> Int
tokLen t = case t of
  IdentTok (Id i) -> T.length i
  ConIdentTok (Id i) -> T.length i
  WildId (Id i) -> T.length i
  StringTok s -> T.length s + 2
  IntTok i -> length (show i)
  Unit -> 2
  Comma -> 1
  Bang -> 1
  Arrow -> 2
  Equals -> 1
  Bar -> 1
  LBrace -> 1
  RBrace -> 1
  LBracket -> 1
  RBracket -> 1
  LAngle -> 1
  RAngle -> 1
  Colon -> 1
  LParen -> 1
  RParen -> 1
  Interface -> 9
  Data -> 4
  Let -> 3
  LetRec -> 6
  In -> 2
