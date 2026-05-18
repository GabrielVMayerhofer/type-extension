module Main (main) where

import Test.HUnit
import Control.Monad.State (evalStateT)
import AST
import TypeChecker

run :: Expr -> Either String Type
run e = evalStateT (checker e) []

-- Bool literals
testTrue :: Test
testTrue = TestCase $ assertEqual "ETrue has type TBool" (Right TBool) (run ETrue)

testFalse :: Test
testFalse = TestCase $ assertEqual "EFalse has type TBool" (Right TBool) (run EFalse)

-- Nat literals
testZero :: Test
testZero = TestCase $ assertEqual "Zero has type TNat" (Right TNat) (run Zero)

testSuccZero :: Test
testSuccZero = TestCase $ assertEqual "Succ Zero has type TNat" (Right TNat) (run (Succ Zero))

testPredSuccZero :: Test
testPredSuccZero = TestCase $ assertEqual "Pred (Succ Zero) has type TNat" (Right TNat) (run (Pred (Succ Zero)))

testSuccNested :: Test
testSuccNested = TestCase $ assertEqual "Succ (Succ Zero) has type TNat" (Right TNat) (run (Succ (Succ Zero)))

-- IsZero
testIsZeroZero :: Test
testIsZeroZero = TestCase $ assertEqual "IsZero Zero has type TBool" (Right TBool) (run (IsZero Zero))

testIsZeroSucc :: Test
testIsZeroSucc = TestCase $ assertEqual "IsZero (Succ Zero) has type TBool" (Right TBool) (run (IsZero (Succ Zero)))

-- If expressions (well-typed)
testIfBoolBranches :: Test
testIfBoolBranches = TestCase $
  assertEqual "if true then true else false : TBool"
    (Right TBool)
    (run (If ETrue ETrue EFalse))

testIfNatBranches :: Test
testIfNatBranches = TestCase $
  assertEqual "if false then 0 else succ 0 : TNat"
    (Right TNat)
    (run (If EFalse Zero (Succ Zero)))

testIfCondIsZero :: Test
testIfCondIsZero = TestCase $
  assertEqual "if iszero 0 then 0 else succ 0 : TNat"
    (Right TNat)
    (run (If (IsZero Zero) Zero (Succ Zero)))

-- If expressions (ill-typed)
testIfNonBoolCond :: Test
testIfNonBoolCond = TestCase $
  case run (If Zero ETrue EFalse) of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

testIfBranchMismatch :: Test
testIfBranchMismatch = TestCase $
  case run (If ETrue Zero EFalse) of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

-- Succ / Pred on non-Nat (ill-typed)
testSuccBool :: Test
testSuccBool = TestCase $
  case run (Succ ETrue) of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

testPredBool :: Test
testPredBool = TestCase $
  case run (Pred EFalse) of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

-- IsZero on non-Nat (ill-typed)
testIsZeroBool :: Test
testIsZeroBool = TestCase $
  case run (IsZero ETrue) of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

-- Var
testVarUnbound :: Test
testVarUnbound = TestCase $
  case run (Var "x") of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

-- Abs (well-typed)
testAbsIdentityBool :: Test
testAbsIdentityBool = TestCase $
  assertEqual "\\x:Bool. x : Bool -> Bool"
    (Right (TBool `TArrow` TBool))
    (run (Abs ("x", TBool) (Var "x")))

testAbsIdentityNat :: Test
testAbsIdentityNat = TestCase $
  assertEqual "\\x:Nat. x : Nat -> Nat"
    (Right (TNat `TArrow` TNat))
    (run (Abs ("x", TNat) (Var "x")))

testAbsConstant :: Test
testAbsConstant = TestCase $
  assertEqual "\\x:Bool. zero : Bool -> Nat"
    (Right (TBool `TArrow` TNat))
    (run (Abs ("x", TBool) Zero))

testAbsNested :: Test
testAbsNested = TestCase $
  assertEqual "\\x:Bool. \\y:Nat. x : Bool -> Nat -> Bool"
    (Right (TBool `TArrow` (TNat `TArrow` TBool)))
    (run (Abs ("x", TBool) (Abs ("y", TNat) (Var "x"))))

-- App (well-typed)
testAppIdentityBool :: Test
testAppIdentityBool = TestCase $
  assertEqual "(\\x:Bool. x) true : TBool"
    (Right TBool)
    (run (App (Abs ("x", TBool) (Var "x")) ETrue))

testAppIdentityNat :: Test
testAppIdentityNat = TestCase $
  assertEqual "(\\x:Nat. x) zero : TNat"
    (Right TNat)
    (run (App (Abs ("x", TNat) (Var "x")) Zero))

testAppReturnsBool :: Test
testAppReturnsBool = TestCase $
  assertEqual "(\\x:Nat. isZero x) zero : TBool"
    (Right TBool)
    (run (App (Abs ("x", TNat) (IsZero (Var "x"))) Zero))

-- App (ill-typed)
testAppNotAFunction :: Test
testAppNotAFunction = TestCase $
  case run (App ETrue EFalse) of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

testAppArgMismatch :: Test
testAppArgMismatch = TestCase $
  case run (App (Abs ("x", TBool) (Var "x")) Zero) of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

-- Pairs (well-typed)
testPairBoolNat :: Test
testPairBoolNat = TestCase $
  assertEqual "(true, zero) : Bool × Nat"
    (Right (TPair TBool TNat))
    (run (EPair ETrue Zero))

testPairNatBool :: Test
testPairNatBool = TestCase $
  assertEqual "(zero, false) : Nat × Bool"
    (Right (TPair TNat TBool))
    (run (EPair Zero EFalse))

testFstPair :: Test
testFstPair = TestCase $
  assertEqual "(true, zero).1 : Bool"
    (Right TBool)
    (run (EFst (EPair ETrue Zero)))

testSndPair :: Test
testSndPair = TestCase $
  assertEqual "(true, zero).2 : Nat"
    (Right TNat)
    (run (ESnd (EPair ETrue Zero)))

-- Pairs (ill-typed)
testFstNonPair :: Test
testFstNonPair = TestCase $
  case run (EFst ETrue) of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

testSndNonPair :: Test
testSndNonPair = TestCase $
  case run (ESnd Zero) of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

testPairUnitBool :: Test
testPairUnitBool = TestCase $
  assertEqual "(unit, true) : Unit × Bool"
    (Right (TPair TUnit TBool))
    (run (EPair EUnit ETrue))

testNestedPair :: Test
testNestedPair = TestCase $
  assertEqual "((true, zero), unit) : (Bool × Nat) × Unit"
    (Right (TPair (TPair TBool TNat) TUnit))
    (run (EPair (EPair ETrue Zero) EUnit))

testPairWithFunction :: Test
testPairWithFunction = TestCase $
  assertEqual "(\\x:Bool. x, zero) : (Bool -> Bool) × Nat"
    (Right (TPair (TBool `TArrow` TBool) TNat))
    (run (EPair (Abs ("x", TBool) (Var "x")) Zero))

-- Tuples (well-typed)
testTupleBoolNatUnit :: Test
testTupleBoolNatUnit = TestCase $
  assertEqual "(true, zero, unit) : Bool × Nat × Unit"
    (Right (TTuple [TBool, TNat, TUnit]))
    (run (ETuple [ETrue, Zero, EUnit]))

testTupleProjectionFirst :: Test
testTupleProjectionFirst = TestCase $
  assertEqual "(true, zero, unit).1 : Bool"
    (Right TBool)
    (run (EProjIndex (ETuple [ETrue, Zero, EUnit]) 1))

testTupleProjectionSecond :: Test
testTupleProjectionSecond = TestCase $
  assertEqual "(true, zero, unit).2 : Nat"
    (Right TNat)
    (run (EProjIndex (ETuple [ETrue, Zero, EUnit]) 2))

testTupleProjectionThird :: Test
testTupleProjectionThird = TestCase $
  assertEqual "(true, zero, unit).3 : Unit"
    (Right TUnit)
    (run (EProjIndex (ETuple [ETrue, Zero, EUnit]) 3))

-- Tuples (ill-typed)
testTupleProjectionOutOfBounds :: Test
testTupleProjectionOutOfBounds = TestCase $
  case run (EProjIndex (ETuple [ETrue, Zero]) 3) of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

testTupleProjectionNonTuple :: Test
testTupleProjectionNonTuple = TestCase $
  case run (EProjIndex ETrue 1) of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

-- Records (well-typed)
testRecordBoolNat :: Test
testRecordBoolNat = TestCase $
  assertEqual "{active = true, age = zero} : {active: Bool, age: Nat}"
    (Right (TRecord [("active", TBool), ("age", TNat)]))
    (run (ERecord [("active", ETrue), ("age", Zero)]))

testRecordProjectionBool :: Test
testRecordProjectionBool = TestCase $
  assertEqual "{active = true, age = zero}.active : Bool"
    (Right TBool)
    (run (EProjLabel (ERecord [("active", ETrue), ("age", Zero)]) "active"))

testRecordProjectionNat :: Test
testRecordProjectionNat = TestCase $
  assertEqual "{active = true, age = zero}.age : Nat"
    (Right TNat)
    (run (EProjLabel (ERecord [("active", ETrue), ("age", Zero)]) "age"))

testRecordWithPairAndTuple :: Test
testRecordWithPairAndTuple = TestCase $
  assertEqual "{pair = (true, zero), tuple = (true, zero, unit)}"
    (Right
      (TRecord
        [ ("pair", TPair TBool TNat)
        , ("tuple", TTuple [TBool, TNat, TUnit])
        ]))
    (run
      (ERecord
        [ ("pair", EPair ETrue Zero)
        , ("tuple", ETuple [ETrue, Zero, EUnit])
        ]))

testRecordProjectionPair :: Test
testRecordProjectionPair = TestCase $
  assertEqual "{pair = (true, zero)}.pair : Bool × Nat"
    (Right (TPair TBool TNat))
    (run
      (EProjLabel
        (ERecord [("pair", EPair ETrue Zero)])
        "pair"))

-- Records (ill-typed)
testRecordProjectionMissingField :: Test
testRecordProjectionMissingField = TestCase $
  case run (EProjLabel (ERecord [("active", ETrue), ("age", Zero)]) "name") of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

testRecordProjectionNonRecord :: Test
testRecordProjectionNonRecord = TestCase $
  case run (EProjLabel ETrue "active") of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)

testRecordDuplicateLabels :: Test
testRecordDuplicateLabels = TestCase $
  case run (ERecord [("age", Zero), ("age", ETrue)]) of
    Left _  -> return ()
    Right t -> assertFailure ("expected type error, got " ++ show t)


tests :: Test
tests = TestList
  [ TestLabel "ETrue"                testTrue
  , TestLabel "EFalse"               testFalse
  , TestLabel "Zero"                 testZero
  , TestLabel "Succ Zero"            testSuccZero
  , TestLabel "Pred (Succ Zero)"     testPredSuccZero
  , TestLabel "Succ (Succ Zero)"     testSuccNested
  , TestLabel "IsZero Zero"          testIsZeroZero
  , TestLabel "IsZero (Succ Zero)"   testIsZeroSucc
  , TestLabel "If bool branches"     testIfBoolBranches
  , TestLabel "If nat branches"      testIfNatBranches
  , TestLabel "If iszero cond"       testIfCondIsZero
  , TestLabel "If non-bool cond"     testIfNonBoolCond
  , TestLabel "If branch mismatch"   testIfBranchMismatch
  , TestLabel "Succ Bool"            testSuccBool
  , TestLabel "Pred Bool"            testPredBool
  , TestLabel "IsZero Bool"          testIsZeroBool
  , TestLabel "Var unbound"          testVarUnbound
  , TestLabel "Abs identity Bool"    testAbsIdentityBool
  , TestLabel "Abs identity Nat"     testAbsIdentityNat
  , TestLabel "Abs constant"         testAbsConstant
  , TestLabel "Abs nested"           testAbsNested
  , TestLabel "App identity Bool"    testAppIdentityBool
  , TestLabel "App identity Nat"     testAppIdentityNat
  , TestLabel "App returns Bool"     testAppReturnsBool
  , TestLabel "App not a function"   testAppNotAFunction
  , TestLabel "App arg mismatch"     testAppArgMismatch

  --Pairs
  , TestLabel "Pair Bool Nat"        testPairBoolNat
  , TestLabel "Pair Nat Bool"        testPairNatBool
  , TestLabel "Fst Pair"             testFstPair
  , TestLabel "Snd Pair"             testSndPair
  , TestLabel "Fst Non-Pair"         testFstNonPair
  , TestLabel "Snd Non-Pair"         testSndNonPair
  , TestLabel "Pair Unit Bool"       testPairUnitBool
  , TestLabel "Nested Pair"          testNestedPair
  , TestLabel "Pair With Function"   testPairWithFunction

  --Tuples
  , TestLabel "Tuple Bool Nat Unit"        testTupleBoolNatUnit
  , TestLabel "Tuple Projection First"     testTupleProjectionFirst
  , TestLabel "Tuple Projection Second"    testTupleProjectionSecond
  , TestLabel "Tuple Projection Third"     testTupleProjectionThird
  , TestLabel "Tuple Projection OOB"       testTupleProjectionOutOfBounds
  , TestLabel "Tuple Projection Non-Tuple" testTupleProjectionNonTuple
  --Records
  , TestLabel "Record Bool Nat"              testRecordBoolNat
  , TestLabel "Record Projection Bool"       testRecordProjectionBool
  , TestLabel "Record Projection Nat"        testRecordProjectionNat
  , TestLabel "Record With Pair And Tuple"   testRecordWithPairAndTuple
  , TestLabel "Record Projection Pair"       testRecordProjectionPair
  , TestLabel "Record Missing Field"         testRecordProjectionMissingField
  , TestLabel "Record Projection Non-Record" testRecordProjectionNonRecord
  , TestLabel "Record Duplicate Labels"      testRecordDuplicateLabels
  ]

main :: IO ()
main = do
  result <- runTestTT tests
  if errors result + failures result > 0
    then fail "Some tests failed."
    else return ()