{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Framer.Templates.ReadMe where

import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)
import Data.String.Interpolate (i)
import Framer.Config

-- | TODO Generalize over author name and current year
readMeText :: Config -> ByteString
readMeText Config {..} =
  fromString
    [i|# #{projectName}
|]
