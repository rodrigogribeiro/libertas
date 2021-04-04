> module Interactive.InputParser where

> import           Core.Syntax
> import           Core.Parser
> import           Interactive.Tactic.Elab
> import           Interactive.Tactic.Parser

> import           Text.Parsec
> import           Text.Parsec.Language
> import           Text.Parsec.String   (Parser)
> import qualified Text.Parsec.Expr as Ex
> import qualified Text.Parsec.Token as Tok

Exported parsing functions
--------------------------

> parseModule :: String -> Either ParseError [Theorem]
> parseModule = parse fileParser ""


Definition of the parser
------------------------

* libertas file parser

> fileParser :: Parser [Theorem]
> fileParser
>       = endBy1 theoremParser (Tok.symbol lexer ";")

* theorem parser

> theoremParser :: Parser Theorem
> theoremParser
>       = f <$> nameP <*> reservedOp "::"
>                     <*> typeParser
>                     <*> reservedOp ":="
>                     <*> body
>      where
>        f n _ t _ e = Theorem n t e


* body parser

> body :: Parser Term
> body = expr <|> (elab <$> parseScript)
