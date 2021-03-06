> module Interactive.Tactic.Parser where

> import           Core.Parser
> import           Core.Syntax
> import           Interactive.Tactic.Core

> import           Text.Parsec
> import           Text.Parsec.String (Parser)
> import qualified Text.Parsec.Token as Tok


Parser for tactics scripts
--------------------------

* Defining the tactics parser

> parseScript :: Parser Script
> parseScript
>       = begin *> tactics <* end
>         where
>           begin = reserved "begin"
>           end = reserved "end"
>           semi = Tok.symbol lexer ";"
>           tactics = Script <$> endBy parseTactic semi

> parseTactic :: Parser CoreTac
> parseTactic
>       = choice [ parseExact
>                , parseAssume
>                , parseApply
>                ]

> parseExact :: Parser CoreTac
> parseExact = Exact <$ exact <*> expr
>       where
>         exact = reserved "exact"

> parseAssume :: Parser CoreTac
> parseAssume = Assume <$ assume <*> nameP
>       where
>          assume = reserved "assume"

> parseApply :: Parser CoreTac
> parseApply = f <$> apply <*> expr
>       where
>          apply = reserved "apply"
>          f _ e = Apply e
