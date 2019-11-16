{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Framer.Makefile where

import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)
import Data.String.Interpolate (i)
import Framer.Config

-- | TODO Generalize over author name and current year
makefileText :: Config -> ByteString
makefileText Config {..} =
  fromString
    [i|.PHONY : clean reformat hlint run

REFORMAT = hindent --line-length 76 --sort-imports

run :
        stack build
        stack exec #{projectName}-exe

hlint :
        hlint app src test

reformat :
        find app src test -name '*.hs' -exec $(REFORMAT) \{} \;

clean : # reformat
        stack clean
        find . -name '*~' -delete
        find . -name '#*' -delete
|]
