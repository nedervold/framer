{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Framer.Templates.GitIgnore where

import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)
import Data.String.Interpolate (i)
import Framer.Config

gitIgnoreText :: Config -> ByteString
gitIgnoreText Config {..} =
  fromString
    [i|# stack cruft
.stack-work/
#{projectName}.cabal

# emacs cruft
*~
\#*
|]
  where
    ProjectInfo {..} = projectInfo
