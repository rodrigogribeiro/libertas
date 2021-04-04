> module Interactive.Tactic.Elab where

> import Core.Syntax
> import Interactive.Tactic.Core

Definition of the tactic elaborator
-----------------------------------

> elab :: Script -> Term
> elab (Script ts)
>       = maybe (Var (Name "Elaborator error!"))
>               id
>               (foldr step Nothing ts)
>        where
>          step (Exact e) _ = Just e
>          step (Assume n) (Just e) = Just (Lam n e)
>          step (Apply f a) _ = Just (App f a)
