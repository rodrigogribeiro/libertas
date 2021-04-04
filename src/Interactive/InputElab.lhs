> module Interactive.InputElab where

> import Lens.Micro
> import Core.Syntax

> import Interactive.InputSyntax
> import Interactive.Tactic.Elab

Elaborator
----------

This file contains all functions to convert the
high level syntax into the core.

> elabModule :: Module -> [Definition]
> elabModule m
>       = elabDeclaration <$> (m ^. declarations)

> elabDeclaration :: Declaration -> Definition
> elabDeclaration (Raw n t e) = Definition n t e
> elabDeclaration (Tactics n t s) = Definition n t (tacticElab s)
