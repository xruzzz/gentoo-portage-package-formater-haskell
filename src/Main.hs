{-# LANGUAGE OverloadedStrings, RecordWildCards, UnicodeSyntax #-}
import System.IO(readFile)
import Control.Monad (mapM)
import qualified Data.ByteString as BS (concat, ByteString) 
import qualified Data.ByteString.Char8 as BSC8 (pack, unpack,unwords)
import Text.ParserCombinators.Parsec ( (<|>), anyChar, char, many, noneOf, parse, string, GenParser (..),ParseError(..)) -- as PAR
import Types (Package (..))

pr ∷ GenParser Char st String
pr =    string "="
    <|> string ">="
    <|> string "<="

-- namepkg ∷ String -> GenParser Char st String
namepkg ∷ GenParser Char st String
namepkg = many $ noneOf " \t" -- anyChar -- (oneOf (['A' .. 'Z']++['a' .. 'z']))

use1 ∷ GenParser Char st String
use1 = namepkg

usess = use1

comments ∷ GenParser Char st String
comments = do
    char '#'
    stringSpaces
    sr <- many $ noneOf " \t"
    return sr

stringSpaces ∷ GenParser Char st String
stringSpaces = many (char ' ' <|> char '\t')

mpl ∷ GenParser Char st [String]
mpl = do
    nm <- namepkg
    stringSpaces
    uss <- usess
    return [nm, uss] 
-- lineConf ∷ String -> Either ParseError [String]
lineConf ∷ GenParser Char st [String]
lineConf = do
        mpl
    <|> do
        pr
        mpl
    <|> do
        pr
        mpl
--        stringSpaces
--        comments
    <|> do
        mpl
--        stringSpaces
--        comments

-- import System.Directory

-- parseLine ∷ String -> Either ParseError [[String]]
parseLine ∷ String -> Either ParseError [String]
parseLine = parse lineConf "(unknown)"

testStr1 = "app-arch/bzip2-1.0.6-r7     abi_x86_32"
testStr2 = ">=app-arch/bzip2-1.0.6-r7     abi_x86_32"

main ∷ IO ()
main = do
    contents ← lines <$> readFile "/etc/portage/package.use"
--    putStrLn "1ᚣ"
    let cn = take 5 contents
    print $ parseLine testStr1
    mapM_ putStrLn cn
