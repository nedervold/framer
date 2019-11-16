{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Framer.PackageYaml where

import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)
import Data.String.Interpolate (i)
import Framer.Config

-- | TODO Generalize over author name and current year
packageYamlText :: Config -> ByteString
packageYamlText Config {..} =
  fromString
    [i|name:                #{projectName}
version:             0.1.0.0
github:              "#{githubName}/#{projectName}"
license:             BSD3
author:              "#{authorName}"
maintainer:          "#{authorEmail}"
copyright:           "#{thisYear} #{authorName}"

extra-source-files:
- README.md
- ChangeLog.md

# Metadata used when publishing your package
# synopsis:            Short description of your package
# category:            Web

# To avoid duplicated efforts in documentation and dealing with the
# complications of embedding Haddock markup inside cabal files, it is
# common to point users to the README.md file.
description:         Please see the README on GitHub at <https://github.com/#{githubName}/#{projectName}#readme>

dependencies:
- base >= 4.7 && < 5

library:
  source-dirs: src

executables:
  #{projectName}-exe:
    main:                Main.hs
    source-dirs:         app
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - #{projectName}

tests:
  #{projectName}-test:
    main:                Spec.hs
    source-dirs:         test
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - #{projectName}

|]
