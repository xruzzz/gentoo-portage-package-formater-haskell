{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
import System.IO(readFile)
import Control.Monad (mapM)
import qualified Data.ByteString as BS (concat, ByteString) 
import qualified Data.ByteString.Char8 as BSC8 (pack, unpack,unwords)
import Text.ParserCombinators.Parsec -- as PAR

pr ∷ GenParser Char st String
pr =    string "="
    <|> string ">="
    <|> string "<="

-- namepkg ∷ String -> GenParser Char st String
namepkg∷ GenParser Char st String
namepkg = many (noneOf " \t")

use1 ∷ GenParser Char st String
use1 = many (noneOf " \t")

usess = many use1

comments = do
    char '#'
    stringSpaces
--    string

stringSpaces ∷ GenParser Char st String
stringSpaces = many (char ' ' <|> char '\t')

mpl ∷ GenParser Char st [String]
mpl = do
    namepkg
    stringSpaces
    usess


lineConf = do
        mpl
        stringSpaces
        comments
{-    <|> do
        pr
        mpl
        stringSpaces
        comments
        -}
-- import System.Directory

-- parseLine ∷ String -> Either ParseError [[String]]
parseLine ∷ String -> Either ParseError String
parseLine = parse pr "(unknown)"

testStr1 = "app-arch/bzip2-1.0.6-r7     abi_x86_32"
testStr2 = ">=app-arch/bzip2-1.0.6-r7     abi_x86_32"

main ∷ IO ()
main = do
    contents ← lines <$> readFile "/etc/portage/package.use"
--  putStrLn "ᚣ"
    let cn = take 5 contents
    print $ parseLine testStr1
    mapM_ putStrLn cn
