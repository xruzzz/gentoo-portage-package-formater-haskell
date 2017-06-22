{-# LANGUAGE RecordWildCards, DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings, UnicodeSyntax #-}
module Package.Common.String
    (
        act,
        activeLine,
        ident,
        lexeme,
        pr,
        whitespace
    ) where

import Text.ParserCombinators.Parsec as PCS( (<|>),(<?>), anyChar, char, choice, digit, letter, many, many1, manyTill, skipMany, spaces, noneOf, oneOf, parse, sepBy, string, try, GenParser (..),ParseError(..), Parser(..))
import Text.Parsec as PS( (<|>),(<?>), anyChar, char, choice, digit, letter, many, many1, manyTill, skipMany, spaces, noneOf, oneOf, parse, sepBy, string, try,ParseError(..))
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<$))
import Control.Monad (void)

act ∷ PCS.GenParser Char st String
act = do
    d ← char '#'
    return [d]

activeLine ∷ PCS.GenParser Char st Bool
activeLine = PS.try $ do
            char '#'
            return False
        <|> return True

ident ∷ PCS.GenParser Char st String
ident = PS.many1 (letter <|> digit <|> oneOf "_.,:(){}-@&*|/") >>= return

pr ∷ PCS.GenParser Char st String
pr = do
        prs ← many $ oneOf "=><"
        return prs
    <|> do
        return ""

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

