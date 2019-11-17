{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Framer.Templates.ChangeLog where

import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)
import Data.String.Interpolate (i)
import Framer.Config

changeLogText :: Config -> ByteString
changeLogText Config {..} =
  fromString
    [i|# Changelog for #{projectName}

## Unreleased changes
|]
