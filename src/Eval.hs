module Eval where

--------------------------------------------------------------------------------
import AST
import Data.List
import qualified Data.List.NonEmpty as NE
import Control.Monad
import qualified Data.Map as M
import qualified Data.Text as T
import Token
--------------------------------------------------------------------------------

data Ctx = Ctx
  { valCtx   :: !(M.Map Id ValD)
  , dataCtx  :: !(M.Map Id DataD)
  , ifaceCtx :: !(M.Map Id IfaceD)
  }

emptyCtx :: Ctx
emptyCtx = Ctx
  { valCtx   = M.empty
  , dataCtx  = M.empty
  , ifaceCtx = M.empty
  }

mkCtx :: [Decl] -> Ctx
mkCtx = foldl' addDecl emptyCtx

addDecl :: Ctx -> Decl -> Ctx
addDecl ctx (ValDecl v) = ctx{ valCtx = M.insert (valId v) v (valCtx ctx) }
addDecl ctx (DataDecl d) = ctx{ dataCtx = M.insert (dataId d) d (dataCtx ctx) }
addDecl ctx (IfaceDecl i) = ctx{ ifaceCtx = M.insert (ifaceId i) i (ifaceCtx ctx) }

type Env = M.Map Id Val

data Val
  = ConV Id [Val]
  | SuspendV Comp
  | IntV Int
  deriving (Show)

eval :: Ctx -> Env -> Expr -> Val

eval ctx env (VarE i) =
    case M.lookup i env of
      Just e  -> e
      Nothing ->
        case M.lookup i (valCtx ctx) of
          Just d  -> eval ctx env (valDeclValue d)
          Nothing -> error ("Unbound variable: " ++ show i)

eval ctx env (AppE e0 es0) =
    let
      e  = eval ctx env e0
      es = map (eval ctx env) es0
    in
      case e of
        SuspendV ps -> match ctx env (NE.toList ps) es
        ConV c as -> ConV c (as ++ es)
        IntV{} -> error ("Can't apply to " ++ show e)

eval _ _ (ConE c) = ConV c []

eval _ _ (SuspendE comp) = SuspendV comp

eval ctx env (LetE i _ e1_0 e2_0) =
    let
      e1 = eval ctx env e1_0
    in
      eval ctx (M.insert i e1 env) e2_0

eval ctx env (LetRecE bndrs e_0) =
    let
      env' = M.fromList (map (\(i, _, c) -> (i, SuspendV c)) bndrs)
               `M.union` env
    in
      eval ctx env' e_0

eval _ _ (IntE i) = IntV i

eval ctx env (TupE es_0) =
    let
      es = map (eval ctx env) es_0
    in
      ConV (Id (T.pack ('(' : replicate (length es) ',' ++ ")"))) es

match :: Ctx -> Env -> [([CompPat], Expr)] -> [Val] -> Val
match _ _ [] vals = error ("match: out of patterns to match: " ++ show vals)
match ctx env ((p, e) : ps) vals
  | Just binds <- matchPats p vals
  = eval ctx (binds `M.union` env) e
  | otherwise
  = match ctx env ps vals

matchPats :: [CompPat] -> [Val] -> Maybe (M.Map Id Val)
matchPats ps vs
  | length ps == length vs
  = M.fromList . concat <$> zipWithM matchOne ps vs
  | otherwise
  = Nothing

matchOne :: CompPat -> Val -> Maybe [(Id, Val)]
matchOne (ValPat val_pat) v = matchValPat val_pat v
matchOne (ReqPat req_id ps k) v = undefined -- TODO: not sure how to implement this
matchOne (WildCompPat i) v = undefined -- TODO: not sure what this is about

matchValPat :: ValPat -> Val -> Maybe [(Id, Val)]

matchValPat (ConPat con_id ps) v =
    case v of
      ConV con_id' as ->
        if length ps == length as && con_id == con_id'
          then fmap concat (zipWithM matchValPat ps as)
          else Nothing
      _ -> Nothing

matchValPat (WildValPat i) val = Just [(i, val)]
