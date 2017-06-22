{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards, DeriveDataTypeable, GeneralizedNewtypeDeriving, UnicodeSyntax #-}
module Package.Types.String
    (
        AXTUsesLine(..),
        Comments,
        UsesBlock(..),
        UsesInfo(..)
    ) where

import Control.Lens
-- Формат хранения, который создаёт etc-update
data UsesBlock = UsesBlock {usesLine ∷ UsesInfo, comments ∷ [Comments]}
    deriving Show

data UsesInfo = UsesInfo { active ∷ Bool, predicate, name ∷ String, comment ∷ Comments, uses ∷ [String]}
    deriving Show

-- Строка в новом формате
newtype AXTUsesLine = AXTUses UsesInfo

instance Show AXTUsesLine where
    show (AXTUses (UsesInfo{..})) = "AXT: " ++ (if active == True then " " else "# ") ++ predicate ++ name ++ ass
        where ass = "\t\t\t" ++ (unwords uses) ++ "\t" ++ comment

type Comments = String
