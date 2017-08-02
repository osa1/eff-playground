module Core where

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
  { compPorts :: [ValTy]
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

data PolyTy = PolyTy
  { polyTyForalls :: [Id]
  , poyTyTy :: ValTy
  } deriving (Show)

--------------------------------------------------------------------------------
-- * Terms

data Use
  = MonoVarU Id
  | PolyVarU Id [TyArg]
  | CmdU Id
  | AppU Use [Constr]
  | ConstrU Constr ValTy
  deriving (Show)

data Constr
  = UseC Use
  | ConAppC Id [Constr]
  | LamC [Id] Constr
  | CaseC Use [Alt]
  | HandleC
      [(Interface, [TyArg])] -- adjustments (commands handled on this port)
      Peg -- abilities used
      Use -- scrutinee
      [CmdAlt] -- alts
      Id -- term binder
      Constr -- term alt
  | LetC Id PolyTy Constr Constr
  | LetRecC [(Id, PolyTy, [Id], Constr)] Constr
  deriving (Show)

data Alt = Alt
  { altCon  :: Id
  , altArgs :: [Id]
  , altRhs  :: Constr
  } deriving (Show)

data CmdAlt = CmdAlt
  { cmdAltCon  :: Id
  , cmdAltArgs :: [Id]
  , cmdAltCont :: Id
  , cmdAltRhs  :: Constr
  } deriving (Show)
