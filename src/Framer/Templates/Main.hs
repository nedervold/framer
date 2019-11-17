{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Framer.Templates.Main where

import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)
import Data.String.Interpolate (i)
import Framer.Config

mainText :: Config -> ByteString
mainText Config {..} =
  fromString
    [i|module Main where

import Lib

main :: IO ()
main = someFunc
|]
