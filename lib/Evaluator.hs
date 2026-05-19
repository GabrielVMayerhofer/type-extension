module Evaluator (eval, eval1, isValue) where

import AST

isNumericValue :: Expr -> Bool
isNumericValue Zero = True
isNumericValue (Succ e) = isNumericValue e
isNumericValue _ = False

isValue :: Expr -> Bool
isValue ETrue = True
isValue EFalse = True
isValue e | isNumericValue e = True
isValue (Abs _ _) = True
isValue EUnit = True
isValue (EPair e1 e2) = isValue e1 && isValue e2
isValue (ETuple es) = all isValue es
isValue (ERecord fields) = all (isValue . snd) fields
isValue (ENil _) = True
isValue (ECons eHead eTail) = isValue eHead && isValue eTail
isValue _ = False

subst :: Name -> Expr -> Expr -> Expr
subst x s t = case t of
  Var y -> if x == y then s else t
  Abs (y, ty) body ->
    if x == y
      then Abs (y, ty) body
      else Abs (y, ty) (subst x s body)
  App t1 t2 -> App (subst x s t1) (subst x s t2)
  If t1 t2 t3 -> If (subst x s t1) (subst x s t2) (subst x s t3)
  Succ t1 -> Succ (subst x s t1)
  Pred t1 -> Pred (subst x s t1)
  IsZero t1 -> IsZero (subst x s t1)
  EFix t1 -> EFix (subst x s t1)
  EAscription t1 ty -> EAscription (subst x s t1) ty
  ELet y t1 t2 ->
    if x == y
      then ELet y (subst x s t1) t2
      else ELet y (subst x s t1) (subst x s t2)
  EInl t1 ty -> EInl (subst x s t1) ty
  EInr t1 ty -> EInr (subst x s t1) ty
  ECase t0 y t1 z t2 ->
    ECase
      (subst x s t0)
      y
      (if x == y then t1 else subst x s t1)
      z
      (if x == z then t2 else subst x s t2)
  ETag label t1 ty -> ETag label (subst x s t1) ty
  ECaseVariant t0 branches ->
    ECaseVariant
      (subst x s t0)
      (map
        (\(label, y, body) ->
          if x == y
            then (label, y, body)
            else (label, y, subst x s body))
        branches)
  EPair t1 t2 -> EPair (subst x s t1) (subst x s t2)
  EFst t1 -> EFst (subst x s t1)
  ESnd t1 -> ESnd (subst x s t1)
  ETuple ts -> ETuple (map (subst x s) ts)
  EProjIndex t1 i -> EProjIndex (subst x s t1) i
  ERecord fields -> ERecord (map (\(label, e) -> (label, subst x s e)) fields)
  EProjLabel t1 label -> EProjLabel (subst x s t1) label
  ECons t1 t2 -> ECons (subst x s t1) (subst x s t2)
  EIsNil t1 -> EIsNil (subst x s t1)
  EHead t1 -> EHead (subst x s t1)
  ETail t1 -> ETail (subst x s t1)
  _ -> t

eval1 :: Expr -> Either String Expr
eval1 expr = case expr of
  If ETrue t2 _ -> return t2
  If EFalse _ t3 -> return t3
  If t1 t2 t3 -> If <$> eval1 t1 <*> pure t2 <*> pure t3

  Succ t1 -> Succ <$> eval1 t1
  Pred Zero -> return Zero
  Pred (Succ nv1) | isNumericValue nv1 -> return nv1
  Pred t1 -> Pred <$> eval1 t1
  IsZero Zero -> return ETrue
  IsZero (Succ nv1) | isNumericValue nv1 -> return EFalse
  IsZero t1 -> IsZero <$> eval1 t1

  App (Abs (x, _) t12) v2 | isValue v2 -> return (subst x v2 t12)
  App v1 t2 | isValue v1 -> App v1 <$> eval1 t2
  App t1 t2 -> (`App` t2) <$> eval1 t1

  EFix (Abs (x, ty) t12) ->
    return (subst x (EFix (Abs (x, ty) t12)) t12)
  EFix v1 | isValue v1 ->
    Left "fix expects a lambda abstraction"
  EFix t1 -> EFix <$> eval1 t1

  ECons t1 t2 | not (isValue t1) -> (`ECons` t2) <$> eval1 t1
  ECons v1 t2 | isValue v1 && not (isValue t2) -> ECons v1 <$> eval1 t2

  EIsNil (ENil _) -> return ETrue
  EIsNil (ECons v1 v2) | isValue v1 && isValue v2 -> return EFalse
  EIsNil t1 -> EIsNil <$> eval1 t1

  EHead (ECons v1 v2) | isValue v1 && isValue v2 -> return v1
  EHead (ENil _) -> Left "head of empty list"
  EHead t1 -> EHead <$> eval1 t1

  ETail (ECons v1 v2) | isValue v1 && isValue v2 -> return v2
  ETail (ENil _) -> Left "tail of empty list"
  ETail t1 -> ETail <$> eval1 t1

  _ -> Left "stuck term"

eval :: Expr -> Either String Expr
eval expr
  | isValue expr = return expr
  | otherwise = eval1 expr >>= eval
