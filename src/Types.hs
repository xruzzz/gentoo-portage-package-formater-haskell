{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Types
    (
        Package(..)
    ) where

import qualified Data.ByteString as BS (concat, ByteString) 
import qualified Data.ByteString.Char8 as BSC8 (pack, unpack,unwords)

data Package = Pack {   comment, predicate, name ∷ BS.ByteString, uses ∷ [BS.ByteString]}

instance Show Package where
    show (Pack {..}) = (BSC8.unpack $ BS.concat [predicate, name]) ++ ass
        where ass = "\t\t\t" ++ BSC8.unpack (BSC8.unwords uses) ++ "\t" ++ BSC8.unpack comment
