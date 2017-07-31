module AST where

--------------------------------------------------------------------------------
import Data.List.NonEmpty (NonEmpty (..))
--------------------------------------------------------------------------------
import Token
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- * Types

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
-- * Declarations

data Decl
  = ValDecl ValD
  | IfaceDecl IfaceD
  | DataDecl DataD
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

data DataD = DataD
  { dataId      :: Id
  , dataTyBndrs :: [Id]
  , dataCons    :: NonEmpty Con
  } deriving (Show)

data Con = Con
  { conId   :: Id
  , conArgs :: [ValTy]
  } deriving (Show)

--------------------------------------------------------------------------------
-- * Terms

data Expr
  = VarE Id
  | AppE Expr [Expr]
  | ConE Id
  | SuspendE Comp
  | LetE Id ValTy Expr Expr
  | LetRecE [(Id, ValTy, Comp)] Comp
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
