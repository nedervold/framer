{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Framer.Setup where

import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)
import Data.String.Interpolate (i)
import Framer.Config

setupText :: Config -> ByteString
setupText Config {..} =
  fromString
    [i|import Distribution.Simple
main = defaultMain
|]
