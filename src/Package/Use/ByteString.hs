{-# LANGUAGE RecordWildCards, DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings, UnicodeSyntax #-}
module Package.Use.ByteString
    (
        block,
        commentInFile,
        spaceUse,
        useFile,
        usesInfo,
        usesRest,
        useSymbols,
        usess2,
        uses3,
        uses4
    ) where
import Control.Monad (void)
import qualified Data.ByteString as BS (concat, ByteString) 
import qualified Data.ByteString.Char8 as BSC8 (pack, unpack, unwords)

import Package.Types.ByteString as PT(AXTUsesLine(..), Comments, UsesBlock(..), UsesInfo(..))
import Package.Common.ByteString as PC (activeLine, bsPack, ident, lexeme, nu, pr, whitespace)
import Text.Parsec.Char as PC(endOfLine)
import Text.Parsec.ByteString as PSBSL (GenParser (..), Parser)
import Text.ParserCombinators.Parsec as PCS( (<|>),(<?>), anyChar, char, choice, digit, letter, many, many1, manyTill, skipMany, spaces, noneOf, oneOf, parse, sepBy, string, try, GenParser (..),ParseError(..))
import Text.Parsec as PS( (<|>),(<?>), anyChar, char, choice, digit, letter, lookAhead, many, many1, manyTill, skipMany, spaces, noneOf, oneOf, parse, sepBy, string, try,ParseError(..))

block ∷ PSBSL.GenParser Char st UsesBlock
block = do
{-        ub ← usesBlock
        return ub
        <|> do -}
        ui ← usesInfo
        endOfLine
        return $ UsesBlock ui [nu]

commentInBlockFirst ∷ PSBSL.GenParser Char st Comments
commentInBlockFirst = do
--        many (noneOf (string "\n# "))
        endOfLine
        s ← commentInBlock
        return s

commentInBlock ∷ PSBSL.GenParser Char st Comments
commentInBlock = do
        string "# "
        sr ← many anyChar
        endOfLine
        return $ BSC8.pack sr

commentInFile ∷ PSBSL.GenParser Char st Comments
commentInFile = do
        char '#'
        spaces
        sr ← many $ noneOf "\n"
        return . BSC8.pack $ sr
--    <|> return nu
{-
getUsesInfo ∷ PSBSL.GenParser Char st UsesInfo
getUsesInfo = do
        ui ← usesInfo
        endOfLine
        return ui

newComment ∷ PSBSL.GenParser Char st Comments
newComment = do
        string "# "
        manyTill anyChar $ try (string "-->")
        -}
nullUsesInfo = UsesInfo True nu nu nu []


spaceUse ∷ PSBSL.GenParser Char st BS.ByteString
spaceUse = do
    spaces
    r ← bsPack useSymbols
    return r

useSpace ∷ PSBSL.GenParser Char st BS.ByteString
useSpace = do
    r ← bsPack useSymbols
    spaces
    return r

use1 ∷ PSBSL.GenParser Char st BS.ByteString
use1 = do
    r ← bsPack useSymbols
    return r

useFile ∷ PSBSL.GenParser Char st [UsesBlock]
useFile = do
    ubs ← block `sepBy` endOfLine
    return ubs

usesRest ∷ PSBSL.GenParser Char st [BS.ByteString]
usesRest  = do
        PS.lookAhead $ char '#'
        return []
    <|> do
        spaces
        ur <- many spaceUse
        return ur

useSymbols = many1 (letter <|> digit <|> oneOf "_.,:-") >>= return

usessWork ∷ PSBSL.GenParser Char st [BS.ByteString]
usessWork = do
    fu ← many1 useSpace
--         ur ← usesRest
    return fu

lookUses ∷ PSBSL.GenParser Char st [BS.ByteString]
lookUses = do
        lookAhead endOfLine
        return []
    <|> do
        lookAhead $ (spaces >> oneOf "#=")
        return [] 
    <|> many1 spaceUse

usess2 ∷ PSBSL.GenParser Char st [BS.ByteString]
usess2 = do
    fu ← use1
    ur ← PS.try $ do
                usin ← PS.try . many1 $ do
                        spaces
                        us1 <- (PS.try $ useSymbols) <|> return []
                        return us1
                    <|> return []
                return usin
            <|> return []
    return $ fu:(BSC8.pack <$> ur)

-- Не отлажена
uses3 ∷ PSBSL.GenParser Char st [BS.ByteString]
uses3 = do
    sd <- (many1 useSymbols) `sepBy` spaces
    return $ BSC8.pack <$> (head sd)

-- Проходит тесты
uses4 ∷ PSBSL.GenParser Char st [BS.ByteString]
uses4 = do
    fu ← use1
    ur ←{- PS.try $ do
            spaces
            char '#'
            return []
            <|> -}
        PS.try $ do
            usesRecursion
        <|> return []
    return $ fu:ur

usesRecursion ∷ PSBSL.GenParser Char st [BS.ByteString]
usesRecursion = do
        spaces
        us <- do {-lookAhead $ do
                endOfLine
                return []
            <|> do
                lookAhead $ do
                char '#'
                return []
                <|> -}
            
                uss <- uses4
                return uss
        return us

usesInfo ∷ PSBSL.GenParser Char st UsesInfo
usesInfo = do
{-        ac  ← startUsesInfoLine
        prs ← pr
        nm  ← bsPack ident
        spaces
        uss ← usess -- (\x -> [x]) <$> simpleUses
        spaces
        cm  ← commentInFile
        return $ UsesInfo ac prs nm cm uss
        <|> do -}
        ac  ← PC.activeLine
        prs ← PC.pr
        nm  ← bsPack PC.ident
        spaces
{-        fu ← use1
spaces-}
        us ← uses4
{-            <|> lookAhead $ do
                endOfLine
                return []
            <|> do
                uss <- uses4
                return uss-}
        cm ← PS.try $ do
                many $ oneOf " \t"
                cmin ← PS.try (commentInFile >>= return) <|> return nu
                return cmin
                <|> return nu
        return $ UsesInfo ac prs nm nu us
    <?> "тест вопроса ?"

usesBlock ∷ PSBSL.GenParser Char st UsesBlock
usesBlock = do
        lc ← commentInBlockFirst
--        commentInBlockOther
--        commentInBlockOther
--        ui ← getUsesInfo
        return $ UsesBlock nullUsesInfo [lc]
{- do
                uss <- PU.usess2
                return (uss, PC.nu)-}
