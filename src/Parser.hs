{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Ast
import Control.Monad.Combinators.Expr
import Data.Functor
import Data.Text
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

parseTestEof p = parseTest (p <* eof)

pExpr :: Parser Expr
pExpr =
  makeExprParser
    pTerm
    [ [InfixL (symbol "*" $> Mul), InfixL (symbol "/" $> Div)],
      [InfixL (symbol "+" $> Add), InfixL (symbol "-" $> Sub)]
    ]
    <?> "expression"

pTerm :: Parser Expr
pTerm = choice [parens pExpr, Lit <$> pLit]

pLit :: Parser Lit
pLit = choice [pNum, pBool, pString]
  where
    pNum = Num <$> lexeme L.decimal <?> "interger"
    pBool = (keyword "True" $> Bool True) <|> (keyword "False" $> Bool False) <?> "boolean"
    pString = String <$> between (char '\'') (char '\'') (pack <$> many (anySingleBut '\'')) <?> "string"

lexeme = L.lexeme space

symbol = L.symbol space

keyword :: Text -> Parser Text
keyword k = lexeme (string k) <* notFollowedBy alphaNumChar

parens = between (symbol "(") (symbol ")")

braces = between (symbol "{") (symbol "}")

semicolon = symbol ";"
