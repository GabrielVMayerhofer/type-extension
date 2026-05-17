module AST where

type Name = String

data Expr = ETrue
          | EFalse
          | If {cond :: Expr, exprThen :: Expr, exprElse :: Expr}
          | Zero
          | Succ Expr
          | Pred Expr
          | IsZero Expr
          | Var Name                  -- x               vars in Lambda Calculus
          | Abs (Name, Type) Expr     -- (\x:T . expr)   abstraction in Lambda Calculus
          | App Expr Expr             -- t1 t2           application in Lambda Calculus
          | EUnit                     -- unit type
          | EAscription Expr Type     -- ascription (t as T)
          | ELet Name Expr Expr       -- let x = t1 in t2
     deriving (Eq, Show)

data Value = VTrue
           | VFalse
           | VZero
           | VSucc Value
           | VAbs (Name, Type) Expr
           | VUnit                    -- singleton unit value
     deriving (Eq, Show)

data Type = TBool
          | TNat
          | Type `TArrow` Type
          | TUnit                     -- singleton unit type
          | TBase String              -- base type (String, Float, etc)
     deriving (Eq, Show)
