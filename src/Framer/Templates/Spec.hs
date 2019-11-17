{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Framer.Templates.Spec where

import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)
import Data.String.Interpolate (i)
import Framer.Config

specText :: Config -> ByteString
specText Config {..} =
  fromString $
  if tastyDiscoverTests
    then [i|{-# OPTIONS_GHC -F -pgmF tasty-discover #-}
|]
    else [i|main :: IO ()
main = putStrLn "Test suite not yet implemented"
|]
