{-# LANGUAGE RecordWildCards, DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings, UnicodeSyntax #-}
module Package.Use.String
    (
        commentInFile,
        usesInfo,
        uses5,
        useTry
    ) where
import Control.Monad (void)
import qualified Data.ByteString as BS (concat, ByteString) 
import qualified Data.ByteString.Char8 as BSC8 (pack, unpack, unwords)

import Package.Types.String as PT(AXTUsesLine(..), UsesBlock(..), UsesInfo(..))
import Package.Common.String as PC (activeLine, ident, lexeme, pr, whitespace)
import Text.Parsec.Char as PC(endOfLine)
import Text.ParserCombinators.Parsec as PCS( (<|>),(<?>), anyChar, char, choice, digit, letter, many, many1, manyTill, skipMany, spaces, noneOf, oneOf, parse, sepBy, GenParser (..),ParseError(..))
import Text.Parsec as PS( (<|>),(<?>), anyChar, char, choice, digit, letter, lookAhead, many, many1, manyTill, skipMany, spaces, noneOf, oneOf, parse, sepBy, string, try,ParseError(..))

block ∷ PCS.GenParser Char st UsesBlock
block = do
        ui ← usesInfo
        endOfLine
        return $ UsesBlock ui [""]

commentInBlockFirst ∷ PCS.GenParser Char st String
commentInBlockFirst = do
        endOfLine
        s ← commentInBlock
        return s

commentInBlock ∷ PCS.GenParser Char st String
commentInBlock = do
        string "# "
        sr ← many anyChar
        endOfLine
        return sr

commentInFile ∷ PCS.GenParser Char st String
commentInFile = do
        char '#'
        spaces
        sr ← many1 $ noneOf "\n"
        return sr

nullUsesInfo = UsesInfo True "" "" "" []


spaceUse ∷ PCS.GenParser Char st String
spaceUse = do
    spaces
    r ← useSymbols
    return r

use1 ∷ PCS.GenParser Char st String
use1 = do
    r ← useSymbols
    return r

useFile ∷ PCS.GenParser Char st [UsesBlock]
useFile = do
    ubs ← block `sepBy` endOfLine
    return ubs

usesRest ∷ PCS.GenParser Char st [String]
usesRest  = do
        PS.lookAhead $ char '#'
        return []
    <|> do
        spaces
        ur <- many spaceUse
        return ur

useSymbols = many1 (letter <|> digit <|> oneOf "_.,:-") >>= return

-- beginComment = string "# required by "

lookUses ∷ PCS.GenParser Char st [String]
lookUses = do
        lookAhead endOfLine
        return []
    <|> do
        lookAhead $ (spaces >> oneOf "#=")
        return [] 
    <|> many1 spaceUse

useTry ∷ Bool -> ([String], String) -> PCS.GenParser Char st ([String], String)
useTry False ss = return ss
useTry True (acc, _) = do
            uss <- useSymbols
            spaces
            (cmm, ds) <- PS.try $ do
                        cm <- commentInFile
                        return (cm, False)
                <|> do
                    return ("", True)
                        {- PS.lookAhead $ do
                                -- (oneOf "#\n")
                                return False
                                <|> return True -}
            useTry ds (acc ++ [uss], cmm)

-- Проходит тесты
uses4 ∷ PCS.GenParser Char st [String]
uses4 = do
    fu ← use1
    ur ← PS.try $ do
            usesRecursion
        <|> return []
    return $ fu:ur

uses5 ∷ PCS.GenParser Char st [String]
uses5 = (many $ noneOf "# \t\n") `sepBy` spaces

usesRecursion ∷ PCS.GenParser Char st [String]
usesRecursion = do
        spaces
        us <- do
                uss <- uses4
                return uss
        return us

usesInfo ∷ PCS.GenParser Char st UsesInfo
usesInfo = do
        ac  ← PC.activeLine
        prs ← PC.pr
        nm  ← PC.ident
        spaces
        usess1 <- many1 (noneOf "#")
--        (us, cm) ← useTry True ([], "")
        cm ← PS.try (commentInFile >>= return)
            <|> return ""
        return $ UsesInfo ac prs nm cm $ words usess1
    <?> "тест вопроса ?"

usesBlock ∷ PCS.GenParser Char st UsesBlock
usesBlock = do
        lc ← commentInBlockFirst
        return $ UsesBlock nullUsesInfo [lc]

