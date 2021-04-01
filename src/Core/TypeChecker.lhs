> module Core.TypeChecker where

> import           Control.Monad.Reader
> import           Control.Monad.Except
> import           Data.Either (partitionEithers)
> import           Data.Functor.Identity
> import           Data.Map (Map)
> import qualified Data.Map as Map
> import           Core.Syntax

Proof checking by bi-directional type checking
----------------------------------------------

A type for the major errors that can be found in type checking

> data Error
>       = ExpectedFunction
>       | UndefinedVariable Name
>       | MatchingError Ty Ty
>       | InferenceError Term
>       deriving (Eq, Ord, Show)


Running the proof checker

> checkModule :: [Theorem] -> Either [Error] [Theorem]
> checkModule ts
>       = case partitionEithers (map checkTheorem ts) of
>           (es@(_ : _) , _) -> Left es
>           _                -> Right ts

> checkTheorem :: Theorem -> Either Error Ty
> checkTheorem (Theorem _ t p) = checkProof p t

> checkProof :: Term -> Ty -> Either Error Ty
> checkProof e t = runTcM check Map.empty
>      where
>        check
>           = do
>               typeCheck e t
>               return t


A monad for type checking

> type TcM a = (ReaderT Ctx (ExceptT Error Identity)) a

> runTcM :: TcM a -> Ctx -> Either Error a
> runTcM tcm ctx = runIdentity $ runExceptT (runReaderT tcm ctx)

A simple utility function for matching function types

> splitArrow :: Ty -> TcM (Ty,Ty)
> splitArrow (Arrow l r)
>       = return (l,r)
> splitArrow _
>       = throwError ExpectedFunction

type checking and type inference algorithms

> typeCheck :: Term -> Ty -> TcM ()
> typeCheck (Lam v e) (Arrow l r)
>       = local (Map.insert v l) (typeCheck e r)
> typeCheck e@(Lam _ _) _
>       = throwError ExpectedFunction
> typeCheck e t
>       = do
>           t' <- typeInfer e
>           if t == t' then return ()
>             else throwError (MatchingError t t')

> typeInfer :: Term -> TcM Ty
> typeInfer e@(Lam _ _)
>       = throwError (InferenceError e)
> typeInfer (Ann e t)
>       = do
>           typeCheck e t
>           return t
> typeInfer (App e e')
>       = do
>           t <- typeInfer e
>           (l,r) <- splitArrow t
>           typeCheck e' l
>           return r
> typeInfer (Var n)
>       = do
>           ty <- reader (Map.lookup n)
>           case ty of
>             Just t -> return t
>             _      -> throwError (UndefinedVariable n)
