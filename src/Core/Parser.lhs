> module Core.Parser where

> import           Core.Syntax

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
>       = endBy theoremParser (Tok.symbol lexer ";")

* theorem parser

> theoremParser :: Parser Theorem
> theoremParser
>       = f <$> nameP <*> reservedOp "::"
>                     <*> typeParser
>                     <*> reservedOp ":="
>                     <*> expr
>      where
>        f n _ t _ e = Theorem n t e

* Syntax parsing for types

> typeParser :: Parser Ty
> typeParser
>       = Ex.buildExpressionParser table atomic
>         where
>           table = [[binary "->" Arrow Ex.AssocRight]]
>           binary nm fun assoc
>               = Ex.Infix (do {
>                         reservedOp nm ;
>                         return fun})
>                       assoc

> atomic :: Parser Ty
> atomic = parens typeParser       <|>
>          (F <$ reserved "false") <|>
>          (TVar <$> nameP)

* Syntax parsing for terms

> nameP :: Parser Name
> nameP = Name <$> identifier

> termVar :: Parser Term
> termVar = Var <$> nameP

> termLam :: Parser Term
> termLam
>       = do
>            reservedOp "\\"
>            vs <- many1 nameP
>            reservedOp "."
>            e <- expr
>            return (foldr Lam e vs)

> term :: Parser Term
> term = parens expr <|>
>        termLam     <|>
>        termVar

> expr :: Parser Term
> expr = do
>          es <- many1 term
>          return $ foldl1 App es

* Basic lexing stuff

> lexer :: Tok.TokenParser ()
> lexer = Tok.makeTokenParser style
>    where
>      ops = ["\\", ".", "->", ":=", "::"]
>      names = ["false"]
>      style = emptyDef{ Tok.reservedOpNames = ops
>                      , Tok.reservedNames = names
>                      , Tok.commentLine = "#" }

> reserved :: String -> Parser ()
> reserved = Tok.reserved lexer

> reservedOp :: String -> Parser ()
> reservedOp = Tok.reservedOp lexer

> identifier :: Parser String
> identifier = Tok.identifier lexer

> parens :: Parser a -> Parser a
> parens = Tok.parens lexer
