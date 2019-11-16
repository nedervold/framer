{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Framer.PackageYaml where

import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (catMaybes)
import Data.String (IsString(..))
import Data.String.Interpolate (i)
import qualified Data.Vector as V
import Data.Yaml
import Framer.Config

-- | TODO Generalize over author name and current year
packageYamlText :: Config -> ByteString
packageYamlText Config {..} = encode yaml
  where
    yaml :: Value
    yaml =
      Object $
      HM.fromList $
      catMaybes
        [ Just ("author", String $ fromString authorName)
        , Just
            ( "copyright"
            , String $ fromString [i|#{thisYear} #{authorName}|])
        , Just
            ( "dependencies"
            , Array $ V.fromList [String "base >= 4.7 && < 5"])
        , Just
            ( "description"
            , String $
              fromString
                [i|Please see the README on GitHub at <https://github.com/#{githubName}/#{projectName}#readme>|])
        , Just
            ( "extra-source-files"
            , Array $ V.fromList [String "README.md", String "ChangeLog.md"])
        , Just ("ghc-options", String "-Wall -Wcompat")
        , Just
            ( "github"
            , String $ fromString [i|#{githubName}/#{projectName}|])
        , Just ("license", String "BSD3")
        , Just ("maintainer", String $ fromString authorEmail)
        , Just ("name", String $ fromString projectName)
        , Just ("version", String "0.1.0.0")
        , fmap ("library", ) mLibrary
        , if null mExecutables
            then Nothing
            else Just $ ("executables", Object $ HM.fromList $ mExecutables)
        , fmap ("tests", ) mTests
        ]
    mLibrary = Just $ Object $ HM.fromList [("source-dirs", String "src")]
    mExecutables =
      [ ( fromString [i|#{projectName}-exe|]
        , Object $
          HM.fromList
            [ ("main", "Main.hs")
            , ("source-dirs", "app")
            , ( "ghc-options"
              , Array $
                V.fromList ["-threaded", "-rtsopts", "-with-rtsopts=-N"])
            , ( "dependencies"
              , Array $ V.fromList [fromString [i|#{projectName}|]])
            ])
      ]
    mTests =
      Just $
      Object $
      HM.fromList
        [ ( fromString [i|#{projectName}-test|]
          , Object $
            HM.fromList
              [ ("main", "Spec.hs")
              , ("source-dirs", "test")
              , ( "ghc-options"
                , Array $
                  V.fromList ["-threaded", "-rtsopts", "-with-rtsopts=-N"])
              , ( "dependencies"
                , Array $ V.fromList [fromString [i|#{projectName}|]])
              ])
        ]
