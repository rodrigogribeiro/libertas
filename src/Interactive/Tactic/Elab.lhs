> module Interactive.Tactic.Elab where

> import Core.Syntax
> import Interactive.Tactic.Core

Definition of the tactic elaborator
-----------------------------------

The tactic elaborator is based on a hole term syntax

> tacticElab :: Script -> Term
> tacticElab (Script ts)
>       = foldr step Hole ts
>       where
>         step (Exact e) _ = e
>         step (Assume n) ac = Lam n ac
>         step (Apply e) ac = App e ac
