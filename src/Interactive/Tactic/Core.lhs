> {-# LANGUAGE DeriveGeneric #-}

> module Interactive.Tactic.Core where

> import           Core.Syntax

> import           GHC.Generics

Definition of the tactic language
---------------------------------

In this file we define the tactic language syntax
and its elaboration into our core semantics.


First, we have the core syntax of our tactics as
a datatype.

> data CoreTac
>   = Exact  Term       -- directly prove a value by using a term.
>   | Assume Name       -- starts the proof of an implication. Generates a lambda.
>   | Apply  Term Term  -- implication elimination. Generates an application.
>   deriving (Eq, Ord, Show, Generic)


A tactic script is just a list of tactics

> data Script
>   = Script [CoreTac]
>     deriving (Eq, Ord, Show, Generic)


