> module Core.Pretty where

> import Core.Syntax

> import Text.PrettyPrint.HughesPJ

Definition of a pretty printer for Libertas core syntax
---------------------------------------------------

* A type class for pretty printing stuff

> class PPrint a where
>   pprint :: a -> Doc

* Main function of pretty printing

> pretty :: PPrint a => a -> String
> pretty = show . pprint

* Instances for the syntax

> instance PPrint Definition where
>   pprint (Definition n t e)
>       = pprint n <+> dcolon <+> pprint t <+> def <+> pprint e
>         where
>           def = text ":="

> instance PPrint Name where
>   pprint (Name s) = text s

> instance PPrint Ty where
>   pprint (TVar n) = pprint n
>   pprint F        = text "false"
>   pprint (Arrow l r)
>     | isArrow l = parens (pprint l) <+> arrow <+> pprint r
>     | otherwise = pprint l <+> arrow <+> pprint r

> instance PPrint Term where
>   pprint (Var n) = pprint n
>   pprint (Lam n e)
>      = lam <+> pprint n <+> dot <+> pprint e
>   pprint (App e e')
>      | isApp e = parens (pprint e) <+> pprint e'
>      | otherwise = pprint e <+> pprint e'
>   pprint (Ann e t) = pprint e <+> dcolon <+> pprint t
>   pprint Hole = text "?"

Auxiliar functions

> isArrow :: Ty -> Bool
> isArrow (Arrow _ _) = True
> isArrow _ = False

> arrow :: Doc
> arrow = text "->"

> lam :: Doc
> lam = text "\\"

> dot :: Doc
> dot = text "."

> isApp :: Term -> Bool
> isApp (Lam _ _) = True
> isApp (App _ _) = True
> isApp _ = False

> dcolon :: Doc
> dcolon = text "::"
