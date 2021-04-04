> {-# LANGUAGE TemplateHaskell #-}

> module Interactive.InputSyntax where

> import Core.Syntax
> import Interactive.Tactic.Core

> import Lens.Micro.TH

Definition of the Libertas input syntax
---------------------------------------

Libertas theories will be organized in modules which
will contain declarations. For now, modules will be
formed by a list of declarations.

* Module definition

> data Module
>       = Module {
>           _declarations :: [Declaration]
>         } deriving (Eq, Ord, Show)

* Declarations

Declarations can be raw term definitions or tactic
scripts which will be elaborated by libertas system.

> data Declaration
>       = Raw { _name  :: Name
>             , _typ   :: Ty
>             ,  _term :: Term }
>       | Tactics { _name   :: Name
>                 , _ty     :: Ty
>                 , _script :: Script }
>       deriving (Eq, Ord, Show)


After parsing, modules will be elaborated into core terms

> makeLenses ''Declaration
> makeLenses ''Module
