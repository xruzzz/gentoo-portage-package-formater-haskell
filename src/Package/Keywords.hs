{-# LANGUAGE RecordWildCards, DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings, UnicodeSyntax #-}
module Package.Keywords
    (

    ) where

import qualified Data.ByteString as BS (concat, ByteString) 
import qualified Data.ByteString.Char8 as BSC8 (pack, unpack, unwords)
