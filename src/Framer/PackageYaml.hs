{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Framer.PackageYaml where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import qualified Data.HashMap.Strict as HM
import Data.String (IsString(..))
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Vector as V
import Data.Yaml
import Framer.Config

-- | TODO Generalize over author name and current year
packageYamlText :: Config -> ByteString
packageYamlText Config {..} = BS.concat [yamlBits, spacer, base]
  where
    spacer =
      "\n\n############################################################\n\n"
    yamlBits = encode yaml
    yaml :: Value
    yaml =
      Object $
      HM.fromList
        [ ("name", String $ fromString projectName)
        , ("version", String "0.1.0.0")
        , ("github", String $ fromString [i|#{githubName}/#{projectName}|])
        , ("license", String "BSD3")
        , ("copyright", String $ fromString [i|#{thisYear} #{authorName}|])
        , ("author", String $ fromString authorName)
        , ("maintainer", String $ fromString authorEmail)
        , ( "extra-source-files"
          , Array $ V.fromList [String "README.md", String "ChangeLog.md"])
        , ( "description"
          , String $
            fromString
              [i|Please see the README on GitHub at <https://github.com/#{githubName}/#{projectName}#readme>|])
        ]
    base =
      BS.fromString
        [i|
# Metadata used when publishing your package
# synopsis:            Short description of your package
# category:            Web

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
