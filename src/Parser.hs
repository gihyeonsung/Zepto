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

pLit :: Parser Lit
pLit = choice [pNum, pBool, pString]
  where
    pNum = Num <$> lexeme L.decimal <?> "interger"
    pBool = (keyword "True" $> Bool True) <|> (keyword "False" $> Bool False) <?> "boolean"
    pString = String <$> between (char '\'') (char '\'') (pack <$> many (anySingleBut '\'')) <?> "string"

keyword :: Text -> Parser Text
keyword k = lexeme (string k) <* notFollowedBy alphaNumChar

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space
