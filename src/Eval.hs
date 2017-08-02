{-# LANGUAGE NamedFieldPuns #-}

module Eval where

--------------------------------------------------------------------------------
import Data.List
import qualified Data.Map as M
import Data.Maybe
--------------------------------------------------------------------------------
import qualified AST
import Core
import Token
--------------------------------------------------------------------------------

data Ctx = Ctx
  { valCtx   :: !(M.Map Id (AST.ValD ValTy Use))
  , dataCtx  :: !(M.Map Id AST.DataD)
  , ifaceCtx :: !(M.Map Id AST.IfaceD)
  }

emptyCtx :: Ctx
emptyCtx = Ctx
  { valCtx   = M.empty
  , dataCtx  = M.empty
  , ifaceCtx = M.empty
  }

{-
mkCtx :: [Decl] -> Ctx
mkCtx = foldl' addDecl emptyCtx

addDecl :: Ctx -> Decl -> Ctx
addDecl ctx (ValDecl v) = ctx{ valCtx = M.insert (valId v) v (valCtx ctx) }
addDecl ctx (DataDecl d) = ctx{ dataCtx = M.insert (dataId d) d (dataCtx ctx) }
addDecl ctx (IfaceDecl i) = ctx{ ifaceCtx = M.insert (ifaceId i) i (ifaceCtx ctx) }
-}

type Env = M.Map Id Val

data Val
  = ConV Id [Val]
  | LamV [Id] Constr
  | IntV Int
  | ContV [Cont]
  deriving (Show)

data Cont
  = C (Ctx -> Env -> Val -> Val)
  | Handle [CmdAlt] Id Constr
  | AppArgs [Constr]
  | AppFn Env Val [Val] [Constr]
  | ConAppArgs Id [Val] [Constr]
  | Case [Alt]
  | Bind Id Constr

instance Show Cont where
  show C{} = "C"
  show Handle{} = "Handle"
  show AppArgs{} = "AppArgs"
  show AppFn{} = "AppFn"
  show ConAppArgs{} = "ConAppArgs"
  show Case{} = "Case"
  show Bind{} = "Bind"

applyCont :: Ctx -> Env -> Val -> [Cont] -> Val
applyCont ctx env v cs0 = case cs0 of
    [] ->
      v
    C c : cs ->
      applyCont ctx env (c ctx env v) cs
    Handle _ x c : cs ->
      evalC ctx (M.insert x v env) c cs
    AppArgs [] : cs ->
      apply ctx env v [] cs
    AppArgs (a : as) : cs ->
      evalC ctx env a (AppFn env v [] as : cs)
    AppFn fn_env fn vs [] : cs ->
      apply ctx fn_env fn (reverse vs) cs
    AppFn fn_env fn vs (a : as) : cs ->
      evalC ctx env a (AppFn fn_env fn (v : vs) as : cs)
    ConAppArgs con vs [] : cs ->
      applyCont ctx env (ConV con (reverse vs)) cs
    ConAppArgs con vs (a : as) : cs ->
      evalC ctx env a (ConAppArgs con (v : vs) as : cs)
    Case alts : cs ->
      selectAlt ctx env v alts cs
    Bind x c : cs ->
      evalC ctx (M.insert x v env) c cs

selectAlt :: Ctx -> Env -> Val -> [Alt] -> [Cont] -> Val
selectAlt ctx env v0 alts0 cs = go alts0
  where
    (con, args) = case v0 of
      ConV con_ args_ -> (con_, args_)
      _ -> error ("Can't scrutinize non-constructor: " ++ show v0)

    go [] = error "Non-exhaustive case"
    go (Alt{altCon,altArgs,altRhs} : as) =
      if altCon == con
        then evalC ctx (M.fromList (zip altArgs args) `M.union` env) altRhs cs
        else go as

apply :: Ctx -> Env -> Val -> [Val] -> [Cont] -> Val
apply ctx env fn as cs = case fn of
    ConV i as0 ->
      applyCont ctx env (ConV i (as0 ++ as)) cs
    LamV as0 rhs ->
      evalC ctx (M.fromList (zip as0 as) `M.union` env) rhs cs
    IntV{} ->
      error ("Can't apply to int: " ++ show fn)
    ContV cs' ->
      case as of
        [a] -> applyCont ctx env a cs'
        _   -> error ("Continuation application to " ++ show (length as) ++ " arguments")

applyCmd :: Ctx -> Env -> Id -> [Val] -> [Cont] -> [Cont] -> Val
applyCmd ctx env cmd cmd_args cs0 acc = case cs0 of
    [] ->
      error ("Can't find handler for cmd: " ++ show cmd)
    h@(Handle alts _ _) : cs ->
      case findCmdAlt cmd alts of
        Nothing ->
          applyCmd ctx env cmd cmd_args cs (h : acc)
        Just CmdAlt{cmdAltArgs,cmdAltCont,cmdAltRhs} ->
          let
            env' = M.insert cmdAltCont (ContV (reverse acc)) $
                   M.union (M.fromList (zip cmdAltArgs cmd_args)) $
                   env
          in
            evalC ctx env' cmdAltRhs cs
    c : cs ->
      applyCmd ctx env cmd cmd_args cs (c : acc)

findCmdAlt :: Id -> [CmdAlt] -> Maybe CmdAlt
findCmdAlt cmd = find ((== cmd) . cmdAltCon)

lookupVar :: Id -> Env -> Val
lookupVar v e = fromMaybe (error ("Unbound variable: " ++ show v)) (M.lookup v e)

evalU :: Ctx -> Env -> Use -> [Cont] -> Val
evalU ctx env u0 cs = case u0 of
    MonoVarU v ->
      applyCont ctx env (lookupVar v env) cs
    PolyVarU v _ ->
      applyCont ctx env (lookupVar v env) cs
    CmdU i ->
      applyCmd ctx env i [] cs []
    AppU u as ->
      evalU ctx env u (AppArgs as : cs)
    ConstrU c _ ->
      evalC ctx env c cs

evalC :: Ctx -> Env -> Constr -> [Cont] -> Val
evalC ctx env c0 cs = case c0 of
    UseC u ->
      evalU ctx env u cs
    ConAppC con [] ->
      applyCont ctx env (ConV con []) cs
    ConAppC con (a : as) ->
      evalC ctx env a (ConAppArgs con [] as : cs)
    LamC args rhs ->
      applyCont ctx env (LamV args rhs) cs
    CaseC scrt alts ->
      evalU ctx env scrt (Case alts : cs)
    HandleC _ _ scrt alts tm_bndr tm_rhs ->
      evalU ctx env scrt (Handle alts tm_bndr tm_rhs : cs)
    LetC x _ c1 c2 ->
      evalC ctx env c1 (Bind x c2 : cs)
    LetRecC binds c ->
      evalC ctx (M.fromList (map (\(i, _, args, rhs) -> (i, LamV args rhs)) binds)
                  `M.union` env) c cs
