-- Модуль для обработки вывода emerge и записи обработанных строк в конфиг файлы
{-# LANGUAGE RecordWildCards, DeriveDataTypeable, GeneralizedNewtypeDeriving, OverloadedStrings, UnicodeSyntax #-}
module OutHandle
    (

    ) where

import qualified Data.ByteString as BS (concat, ByteString) 
import qualified Data.ByteString.Char8 as BSC8 (pack, unpack, unwords)
