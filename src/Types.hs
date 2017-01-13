{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards, DeriveDataTypeable, GeneralizedNewtypeDeriving, UnicodeSyntax #-}
module Types
    (
        AXTUsesLine(..),
        Comments,
        UsesBlock(..),
        UsesInfo(..)
    ) where

import qualified Data.ByteString as BS (concat, ByteString) 
import qualified Data.ByteString.Char8 as BSC8 (pack, unpack,unwords)

-- Формат хранения, который создаёт etc-update
data UsesBlock = UsesBlock {usesLine ∷ UsesInfo, comments ∷ [Comments]}
    deriving Show

data UsesInfo = UsesInfo { active, predicate, name ∷ BS.ByteString, comment ∷ Comments, uses ∷ [BS.ByteString]}
    deriving Show

-- Строка в новом формате
newtype AXTUsesLine = AXTUses UsesInfo
{-
instance Show AXTUsesLine where
    show (AXTUses {..}) = (BSC8.unpack $ BS.concat [predicate, name]) ++ ass
        where ass = "\t\t\t" ++ BSC8.unpack (BSC8.unwords uses) ++ "\t" ++ BSC8.unpack comment
        -}

type Comments = BS.ByteString
