module AST where

data Expr = ETrue
          | EFalse
          | If {cond :: Expr, exprThen :: Expr, exprElse :: Expr}
          | Zero
          | Succ Expr
          | Pred Expr
          | IsZero Expr

data Value = VTrue
           | VFalse
           | VZero
           | VSucc Value

data Type = TBool
          | TNat
