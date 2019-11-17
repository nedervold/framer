{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Framer.Templates.Makefile where

import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)
import Framer.Config
import Text.Printf (printf)

-- | TODO Tabs and backslashes get messed up, either on production or,
-- more likely, when running hindent.
makefileText :: Config -> ByteString
makefileText Config {..} =
  fromString $
  unlines
    [ ".PHONY : clean reformat hlint run"
    , ""
    , "REFORMAT = hindent --line-length 76 --sort-imports"
    , ""
    , "run :"
    , "\tstack build"
    , printf "\tstack exec %s-exe" projectName
    , ""
    , "hlint :"
    , "\thlint app src test"
    , ""
    , "reformat :"
    , "\tfind app src test -name '*.hs' -exec $(REFORMAT) \\{} \\;"
    , ""
    , "clean : # reformat"
    , "\tstack clean"
    , "\tfind . -name '*~' -delete"
    , "\tfind . -name '#*' -delete"
    ]
  where
    ProjectInfo {..} = projectInfo
