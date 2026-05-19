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
          | EFix Expr                 -- fix t
          | EUnit                     -- unit type
          | EAscription Expr Type     -- ascription (t as T)
          | ELet Name Expr Expr       -- let x = t1 in t2
          -- Sums
          | EInl Expr Type            -- inl t as T1+T2
          | EInr Expr Type            -- inr t as T1+T2
          | ECase Expr Name Expr Name Expr -- case t of inl x => t1 | inr y => t2
          -- Variants
          | ETag Name Expr Type       -- <tag=t> as T
          | ECaseVariant Expr [(Name, Name, Expr)] -- case t of <l=x> => t_x
          --Pairs
          | EPair Expr Expr           -- pair: (t1, t2)
          | EFst Expr                 -- first projection: t.1
          | ESnd Expr                 -- second projection: t.2
          --Tuples
          | ETuple [Expr]
          | EProjIndex Expr Int
          --Records
          | ERecord [(Name, Expr)]
          | EProjLabel Expr Name
          -- Lists
          | ENil Type
          | ECons Expr Expr
          | EIsNil Expr
          | EHead Expr
          | ETail Expr
     deriving (Eq, Show)

data Value = VTrue
           | VFalse
           | VZero
           | VSucc Value
           | VAbs (Name, Type) Expr
           | VUnit                    -- singleton unit value
           | VPair Value Value
           | VTuple [Value]
           | VRecord [(Name, Value)]
           | VNil Type
           | VCons Value Value
     deriving (Eq, Show)

data Type = TBool
          | TNat
          | Type `TArrow` Type
          | TUnit                     -- singleton unit type
          | TBase String              -- base type (String, Float, etc)
          -- Sums
          | TSum Type Type            -- T1 + T2
          -- Variants
          | TVariant [(Name, Type)]   -- <l1:T1, ..., ln:Tn>
          --Pairs
          | TPair Type Type
          --Tuples
          | TTuple [Type]
          --Records
          | TRecord [(Name, Type)]
          --Lists
          | TList Type
     deriving (Eq, Show)
