> module Interactive.InputParser where

> import           Core.Syntax
> import           Core.Parser
> import           Interactive.Tactic.Parser
> import           Interactive.InputSyntax

> import           Text.Parsec
> import           Text.Parsec.Language
> import           Text.Parsec.String   (Parser)
> import qualified Text.Parsec.Expr as Ex
> import qualified Text.Parsec.Token as Tok

Exported parsing functions
--------------------------

> parseModule :: String -> Either ParseError Module
> parseModule = parse fileParser ""


Definition of the parser
------------------------

* libertas file parser

> fileParser :: Parser Module
> fileParser
>       = Module <$> endBy1 declarationParser semi
>       where
>         semi = Tok.symbol lexer ";"

* definition parser

> declarationParser :: Parser Declaration
> declarationParser
>       = f <$> nameP <*> reservedOp "::"
>                     <*> typeParser
>                     <*> reservedOp ":="
>                     <*> body
>      where
>        f n _ t _ f = f n t


* declaration body parser

> body :: Parser (Name -> Ty -> Declaration)
> body = raw <|> script
>       where
>         raw = (\ e n t -> Raw n t e) <$> expr
>         script = (\ e n t -> Tactics n t e) <$> parseScript
