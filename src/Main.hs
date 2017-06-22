{-# LANGUAGE OverloadedStrings, RecordWildCards, UnicodeSyntax #-}

import Control.Monad (liftM, mapM, forM_)
import qualified Data.ByteString as BS (ByteString, concat, putStrLn, readFile) 
import qualified Data.ByteString.Char8 as BSC8 (lines, pack, unpack,unwords)
import Package.Types.String as PT (UsesBlock(..), UsesInfo(..))
import Package as P (act, ident, usesInfo)
import System.IO(readFile)
import Text.Parsec.Char as PC(endOfLine)
import Text.ParserCombinators.Parsec as PS( (<|>),(<?>), anyChar, char, choice, digit, letter, lookAhead, many, many1, manyTill, spaces, noneOf, oneOf, parse, sepBy, string, try, GenParser (..),ParseError(..)) -- as PAR
import Text.Parsec.ByteString as PSBSL (GenParser (..), Parser)
import Text.Parsec.Prim (tokens)
import Text.Parsec.Pos (updatePosChar, updatePosString)

-- string1 ∷ String -> PSBSL.GenParser Char st BS.ByteString
-- string1 s = liftM BSC8.pack $ tokens show updatePosString s

parseLine ∷ String -> Either PS.ParseError UsesInfo
parseLine = parse P.usesInfo "(unknown parseLine)"

-- parseBlocks ∷ BS.ByteString -> Either PS.ParseError [UsesBlock]
--parseBlocks = parse (many P.block) "(unknown parseBlocks)"


main ∷ IO ()
main = do
    contents ← readFile "test.use"
    let cn = take 40 $ lines contents
    mapM_ (print . parseLine) cn
{-    print $ parseBlocks contents
    mapM_ BS.putStrLn cn
-}
