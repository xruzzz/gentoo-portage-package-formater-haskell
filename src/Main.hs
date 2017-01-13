{-# LANGUAGE OverloadedStrings, RecordWildCards, UnicodeSyntax #-}
import System.IO(readFile)
import Control.Monad (mapM)
import qualified Data.ByteString as BS (ByteString, concat, putStrLn, readFile) 
import qualified Data.ByteString.Char8 as BSC8 (lines, pack, unpack,unwords)
import Text.ParserCombinators.Parsec as PS( (<|>),(<?>), anyChar, char, digit, letter, many, many1, noneOf, oneOf, parse, try, GenParser (..),ParseError(..)) -- as PAR
import Text.Parsec.ByteString as PSBSL (GenParser (..), Parser)
import Text.Parsec.Prim (tokens)
import Text.Parsec.Pos (updatePosChar, updatePosString)
import Types (AXTUsesLine(..), Comments, UsesBlock(..), UsesInfo(..))
import Control.Monad (liftM)

string ∷ String -> PSBSL.GenParser Char st BS.ByteString
string s = liftM BSC8.pack (tokens show updatePosString s)

act ∷ PSBSL.GenParser Char st BS.ByteString
act = do
    d ← char '#'
    return $ BSC8.pack [d]

pr ∷ PSBSL.GenParser Char st BS.ByteString
pr = do 
        prs ← many $ oneOf "=><"
        return $ BSC8.pack prs
    <|> do
        return nu

-- repack ∷ PSBSL.GenParser Char st BS.ByteString
-- repack = return . BSC8.pack

ident = many1 (letter <|> digit <|> oneOf "_.,:(){}-#@&*|/") >>= return

namepkg ∷ PSBSL.GenParser Char st BS.ByteString
namepkg = ident >>= return . BSC8.pack

use1 ∷ PSBSL.GenParser Char st BS.ByteString
use1 = namepkg

usess ∷ PSBSL.GenParser Char st [BS.ByteString]
usess = many use1

comment1 ∷ PSBSL.GenParser Char st Comments
comment1 = do
        s ← char '#'
--        many $ oneOf " \t"
        sr ← many anyChar
        char '\n'
        return . BSC8.pack $ s:sr
    <|> return nu

commentInEnd ∷ PSBSL.GenParser Char st Comments
commentInEnd = do
        s ← char '#'
        sr ← many anyChar
        return . BSC8.pack $ s:sr
    <|> return nu
{-
stringSpaces ∷ PSBSL.GenParser Char st ()
stringSpaces = do
    many $ oneOf " \t"
    return ()
    -}

mpl ∷ PSBSL.GenParser Char st (BS.ByteString, [BS.ByteString])
mpl = do
    nm ← namepkg
    many $ oneOf " \t"
    uss ← usess
    return (nm, uss)

nu = BSC8.pack ""

startUsesInfoLine ∷ PSBSL.GenParser Char st BS.ByteString
startUsesInfoLine = (char '#' >>= return . BSC8.pack. (\x->[x]))
    <|> return (nu)

lineConf ∷ PSBSL.GenParser Char st UsesInfo
lineConf = do
        ac  ← startUsesInfoLine
        prs ← pr
        nm  ← namepkg
        many $ oneOf " \t"
        uss ← many $ noneOf "#"
        cm  ← commentInEnd
        return $ UsesInfo ac prs nm cm (map BSC8.pack $ words uss)
    <?> "тест вопроса ?"

usesBlock ∷ PSBSL.GenParser Char st UsesBlock
usesBlock = do
    lc ← comment1
    comment1
    comment1
    ui ← lineConf
    return $ UsesBlock ui [lc]

-- parseLine ∷ String -> Either ParseError [[String]]
parseLine ∷ BS.ByteString -> Either PS.ParseError UsesInfo
parseLine = parse (lineConf) "(unknown)"

testStr1 = "app-arch/bzip2-1.0.6-r7     abi_x86_32"
testStr2 = "#>=app-arch/bzip2-1.0.6-r7     abi_x86_32"

main ∷ IO ()
main = do
    contents ← BS.readFile "/etc/portage/package.use"
    let cn = take 100 $ BSC8.lines contents
    mapM_ (print . parseLine) cn
    mapM_ BS.putStrLn cn
