> {-# LANGUAGE TupleSections #-}
> {-# LANGUAGE DeriveGeneric #-}

> module Core.Syntax where

> import Data.Aeson
> import Data.Map (Map)
> import GHC.Generics

Definition of the core language for propositional logic
--------------------------------------------------------

> newtype Name
>       = Name { unName :: String }
>         deriving (Eq, Ord, Show, Generic)

> instance ToJSON Name
> instance FromJSON Name

> data Term
>       = Var Name
>       | Lam Name Term
>       | App Term Term
>       | Ann Term Ty
>       | Hole
>       deriving (Eq, Ord, Show, Generic)

> instance ToJSON Term
> instance FromJSON Term


> data Ty
>      = TVar Name    -- variables
>      | F            -- falsum
>      | Arrow Ty Ty  -- implication
>        deriving (Eq, Ord, Show, Generic)

> instance ToJSON Ty
> instance FromJSON Ty


> data Definition = Definition Name Ty Term
>                deriving (Eq, Ord, Show, Generic)

> instance ToJSON Definition
> instance FromJSON Definition

Typing contexts

> type Ctx = Map Name Ty


