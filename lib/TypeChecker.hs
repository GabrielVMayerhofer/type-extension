module TypeChecker where

import AST

import Control.Monad.State
import Control.Monad.Except (throwError)

-- Either is a pre-defined data type in Haskell.
-- It is often used to deal with computations that might fail, and
-- is defined as:
--
-- data Either a b = Left a
--                 | Right b
--
-- Either is also an instance of Monad. Remember, a Monad
-- is a triple (M a, >>=, return), where M a is any parametric
-- type.
--
-- The Either Monad is likely implemented as:
--
-- instance Monad (Either a) where
--   return = Right
--   Left v >>= f = Left v
--   Right v >>= f = f v
--
-- Our design is to benefit from the Either monad to deal with
-- the situation that a type checker might eventually fail.
--
-- In the symply typed lambda calculos, computations might not
-- only fail, but also manipulate a state. In our case, the
-- state is the type environment (or type context); a sequence
-- of tuples (Name, Type).
--
-- Since the type checker deals with two kinds of side effects
-- (errors and state), we can use the monad transformer StateT
-- to combine both the State and Either monads.
--
-- The state monad has the operations 'get' (to get the environment) and
-- 'put' (to update the environment).

type Env = [(Name, Type)]
type Err = Either String

type Res a = StateT Env Err a
hasDuplicateLabels :: [Name] -> Bool
hasDuplicateLabels [] = False
hasDuplicateLabels (x:xs) = x `elem` xs || hasDuplicateLabels xs

checker :: Expr -> Res Type
checker expr = case expr of
  ETrue -> return TBool
  EFalse -> return TBool

  If e1 e2 e3 ->
    checker e1 >>= \t1 ->
    checker e2 >>= \t2 ->
    checker e3 >>= \t3 ->
    if t1 == TBool
    then if t2 == t3 then return t2 else throwError ("then/else branches have different types: " ++ show t2 ++ " vs " ++ show t3)
    else throwError ("condition of if must be Bool, got " ++ show t1)

  Zero -> return TNat
  Succ e -> checker e >>= \t -> if t == TNat then return TNat else throwError ("succ expects Nat, got " ++ show t)
  Pred e -> checker e >>= \t -> if t == TNat then return TNat else throwError ("pred expects Nat, got " ++ show t)
  IsZero e -> checker e >>= \t -> if t == TNat then return TBool else throwError ("isZero expects Nat, got " ++ show t)

  Var x -> do
    env <- get
    case lookup x env of
      Nothing -> throwError ("variable not in scope: " ++ x)
      Just t -> return t

  Abs (x, t1) e -> do
    env <- get             -- obtains the environment from the state
    put $ (x, t1) : env      -- updates the state with a new environment
    t2 <- checker e        -- checker for 'e' in the new environment
    put env                -- restores the environment
    return $ t1 `TArrow` t2

  App e1 e2 -> do
    t1 <- checker e1
    t2 <- checker e2

    case t1 of
      (t11 `TArrow` t12) -> if t2 == t11 then return t12 else throwError ("argument type mismatch: expected " ++ show t11 ++ ", got " ++ show t2)
      _ -> throwError ("expected a function type, got " ++ show t1)

  -- Rule T-UNIT
  EUnit -> return TUnit

  -- Rule T-ASCRIPT
  EAscription e t -> do
    t' <- checker e
    if t' == t
    then return t
    else throwError ("ascription type mismatch: declared " ++ show t ++ ", but inferred " ++ show t')

  -- Rule T-LET
  ELet x e1 e2 -> do
    t1 <- checker e1
    env <- get
    put $ (x, t1) : env
    t2 <- checker e2
    put env
    return t2

  -- Sums
  EInl e tSum -> case tSum of
    TSum t1 _ -> do
      t <- checker e
      if t == t1
        then return tSum
        else throwError ("inl expects type " ++ show t1 ++ ", got " ++ show t)
    _ -> throwError ("inl type annotation must be a sum type, got " ++ show tSum)

  EInr e tSum -> case tSum of
    TSum _ t2 -> do
      t <- checker e
      if t == t2
        then return tSum
        else throwError ("inr expects type " ++ show t2 ++ ", got " ++ show t)
    _ -> throwError ("inr type annotation must be a sum type, got " ++ show tSum)

  ECase e x e1 y e2 -> do
    t <- checker e
    case t of
      TSum t1 t2 -> do
        env <- get
        put $ (x, t1) : env
        tB1 <- checker e1
        put $ (y, t2) : env
        tB2 <- checker e2
        put env
        if tB1 == tB2
          then return tB1
          else throwError ("case branches have different types: " ++ show tB1 ++ " vs " ++ show tB2)
      _ -> throwError ("case expects a sum type, got " ++ show t)

  -- Variants
  ETag label e tVariant -> case tVariant of
    TVariant fields -> case lookup label fields of
      Just expectedT -> do
        t <- checker e
        if t == expectedT
          then return tVariant
          else throwError ("variant tag " ++ label ++ " expects type " ++ show expectedT ++ ", got " ++ show t)
      Nothing -> throwError ("label " ++ label ++ " not found in variant type " ++ show tVariant)
    _ -> throwError ("tag type annotation must be a variant type, got " ++ show tVariant)

  ECaseVariant e branches -> do
    t <- checker e
    case t of
      TVariant fields -> do
        let branchLabels = map (\(l, _, _) -> l) branches
            fieldLabels = map fst fields
            missingBranches = filter (`notElem` branchLabels) fieldLabels
            extraBranches = filter (`notElem` fieldLabels) branchLabels
        if not (null missingBranches)
          then throwError ("missing branches for labels: " ++ show missingBranches)
          else if not (null extraBranches)
          then throwError ("extra branches for labels not in variant: " ++ show extraBranches)
          else do
            env <- get
            branchTypes <- mapM (\(l, x, eBranch) -> do
                let tArg = case lookup l fields of
                             Just t -> t
                             Nothing -> error "impossible"
                put $ (x, tArg) : env
                tB <- checker eBranch
                return tB
              ) branches
            put env
            case branchTypes of
              [] -> throwError "empty case variant"
              (tFirst:tRest) ->
                if all (== tFirst) tRest
                  then return tFirst
                  else throwError ("variant case branches have different types: " ++ show branchTypes)
      _ -> throwError ("case variant expects a variant type, got " ++ show t)
  -- Rule T-PAIR
  EPair e1 e2 -> do
    t1 <- checker e1
    t2 <- checker e2
    return (TPair t1 t2)

  -- Rule T-FST
  EFst e -> do
    t <- checker e
    case t of
      TPair t1 _ -> return t1
      _ -> throwError ("fst expects a pair, got " ++ show t)

  --Rule T-SND
  ESnd e -> do
    t <- checker e
    case t of
      TPair _ t2 -> return t2
      _ -> throwError ("snd expects a pair, got " ++ show t)
  
  --Rule T-TUPLE
  ETuple es -> do
    ts <- mapM checker es
    return (TTuple ts)

  --Rule T-TUPLE-PROJ
  EProjIndex e i -> do
    t <- checker e
    case t of
      TTuple ts ->
        if i >= 1 && i <= length ts
        then return (ts !! (i-1))
        else throwError ("tuple index out of bounds: " ++ show i)
      _ -> throwError ("tuple projection expects a tuple, got " ++ show t)

    -- Rule T-RECORD
  ERecord fields -> do
    let labels = map fst fields
    if hasDuplicateLabels labels
    then throwError ("record has duplicate labels: " ++ show labels)
    else do
      typedFields <- mapM
        (\(label, e) -> do
            t <- checker e
            return (label, t))
        fields
      return (TRecord typedFields)

  -- Rule T-RECORD-PROJ
  EProjLabel e label -> do
    t <- checker e
    case t of
      TRecord fields ->
        case lookup label fields of
          Just fieldType -> return fieldType
          Nothing -> throwError ("record field not found: " ++ label)
      _ -> throwError ("record projection expects a record, got " ++ show t)
