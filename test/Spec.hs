import Core.Syntax
import Core.TypeChecker

import Data.Either
import Data.Map

import Test.Tasty
import Test.Tasty.HUnit


tests :: TestTree
tests = testGroup "Unit tests"
                  [
                    testCase "identity" $ isRight (checkProof identity idty) @?= True
                  , testCase "const"    $ isRight (checkProof constt consttty) @?= True
                  , testCase "modus ponens" $ isRight (checkProof mp mpty) @?= True
                  ]

x :: Name
x = Name "x"

y :: Name
y = Name "y"

identity :: Term
identity = Lam x (Var x)

a :: Ty
a = TVar (Name "a")

b :: Ty
b = TVar (Name "b")

idty :: Ty
idty = Arrow a a

constt :: Term
constt = Lam x (Lam y (Var x))

consttty :: Ty
consttty = Arrow a (Arrow b a)

mp :: Term
mp = Lam x (Lam y (App (Var y) (Var x)))

mpty :: Ty
mpty = Arrow a (Arrow (Arrow a b) b)


main :: IO ()
main = defaultMain tests
