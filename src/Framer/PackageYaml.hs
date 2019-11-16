{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Framer.PackageYaml where

import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import Data.String (IsString(..))
import Data.String.Interpolate (i)
import Data.Text (Text)
import qualified Data.Vector as V
import Data.Yaml
import Framer.Config

-- | TODO Generalize over author name and current year
packageYamlText :: Config -> ByteString
packageYamlText Config {..} = encode yaml
  where
    yaml :: Value
    yaml =
      mkObject $
      catMaybes
        [ Just ("author", mkString authorName)
        , Just ("copyright", mkString [i|#{thisYear} #{authorName}|])
        , Just ("dependencies", mkArray [mkString "base >= 4.7 && < 5"])
        , Just
            ( "description"
            , mkString
                [i|Please see the README on GitHub at <https://github.com/#{githubName}/#{projectName}#readme>|])
        , Just
            ( "extra-source-files"
            , mkArray [mkString "README.md", mkString "ChangeLog.md"])
        , Just ("ghc-options", mkString "-Wall -Wcompat")
        , Just ("github", mkString [i|#{githubName}/#{projectName}|])
        , Just ("license", mkString "BSD3")
        , Just ("maintainer", mkString authorEmail)
        , Just ("name", mkString projectName)
        , Just ("version", mkString "0.1.0.0")
        , fmap ("library", ) mLibrary
        , if null mExecutables
            then Nothing
            else Just ("executables", mkObject mExecutables)
        , fmap ("tests", ) $
          if tastyDiscoverTests
            then mTastyDiscoverTests
            else mTests
        ]
    mLibrary :: Maybe Value
    mLibrary = Just $ mkObject [("source-dirs", String "src")]
    mExecutables :: [(Text, Value)]
    mExecutables =
      [ ( fromString [i|#{projectName}-exe|]
        , mkObject
            [ ("main", "Main.hs")
            , ("source-dirs", "app")
            , ( "ghc-options"
              , mkArray ["-threaded", "-rtsopts", "-with-rtsopts=-N"])
            , ("dependencies", mkArray [mkString [i|#{projectName}|]])
            ])
      ]
    mTests :: Maybe Value
    mTests =
      Just $
      mkObject
        [ ( fromString [i|#{projectName}-test|]
          , mkObject
              [ ("main", "Spec.hs")
              , ("source-dirs", "test")
              , ( "ghc-options"
                , mkArray ["-threaded", "-rtsopts", "-with-rtsopts=-N"])
              , ("dependencies", mkArray [mkString [i|#{projectName}|]])
              ])
        ]
    mTastyDiscoverTests :: Maybe Value
    mTastyDiscoverTests =
      Just $
      mkObject
        [ ( fromString [i|#{projectName}-test|]
          , mkObject
              [ ("main", "Spec.hs")
              , ("source-dirs", "test")
              , ( "ghc-options"
                , mkArray ["-threaded", "-rtsopts", "-with-rtsopts=-N"])
              , ( "dependencies"
                , mkArray
                    ([ mkString [i|#{projectName}|]
                     , "tasty == 1.2.3"
                     , "tasty-discover == 4.2.1"
                     ] ++
                     concatMap testTypeLibraries (S.toList tastyTestTypes)))
              ])
        ]

testTypeLibraries :: TestType -> [Value]
testTypeLibraries Hedgehog =
  ["hedgehog == 1.0.1", "tasty-hedgehog == 1.0.0.1"]
testTypeLibraries Hspec = ["hspec == 2.7.1", "tasty-hspec == 1.1.5.1"]
testTypeLibraries HUnit = ["HUnit == 1.6.0.0", "tasty-hunit == 0.10.0.2"]
testTypeLibraries QuickCheck =
  ["QuickCheck == 2.13.2", "tasty-quickcheck == 0.10.1"]
testTypeLibraries SmallCheck =
  ["smallcheck == 1.1.5", "tasty-smallcheck == 0.8.1"]
testTypeLibraries Tasty = []

mkObject :: [(Text, Value)] -> Value
mkObject = Object . HM.fromList

mkArray :: [Value] -> Value
mkArray = Array . V.fromList

mkString :: String -> Value
mkString = String . fromString
