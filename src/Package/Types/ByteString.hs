{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards, DeriveDataTypeable, GeneralizedNewtypeDeriving, UnicodeSyntax #-}
module Package.Types.ByteString
    (
        AXTUsesLine(..),
        Comments,
        UsesBlock(..),
        UsesInfo(..)
    ) where

import qualified Data.ByteString as BS (ByteString, concat) 
import qualified Data.ByteString.Char8 as BSC8 (pack, unpack, unwords)
import Control.Lens
-- Формат хранения, который создаёт etc-update
data UsesBlock = UsesBlock {usesLine ∷ UsesInfo, comments ∷ [Comments]}
    deriving Show

data UsesInfo = UsesInfo { active ∷ Bool, predicate, name ∷ BS.ByteString, comment ∷ Comments, uses ∷ [BS.ByteString]}
    deriving Show
{-
instance Show UsesInfo where
    show (UsesInfo{..}) = "OLD: " ++ (BSC8.unpack $ BS.concat [active,predicate, name]) ++ ass
        where ass = "\t\t\t" ++ BSC8.unpack (BSC8.unwords uses) ++ "\t" ++ BSC8.unpack comment
        -}
-- Строка в новом формате
newtype AXTUsesLine = AXTUses UsesInfo

instance Show AXTUsesLine where
    show (AXTUses (UsesInfo{..})) = "AXT: " ++ (if active == True then " " else "# ") ++ (BSC8.unpack $ BS.concat [predicate, name]) ++ ass
        where ass = "\t\t\t" ++ BSC8.unpack (BSC8.unwords uses) ++ "\t" ++ BSC8.unpack comment

type Comments = BS.ByteString
