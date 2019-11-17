{-# LANGUAGE QuasiQuotes #-}

module Framer.ConfigSpec where

import Data.ByteString (ByteString)
import Data.String (IsString(..))
import Data.String.Interpolate (i)
import Data.Yaml
import Framer.Config
import Test.Tasty.Hspec

spec_config :: Spec
spec_config = do
  describe "AuthorInfo" $ do
    it "parses correctly from YAML" $ do
      let ai =
            case decodeEither' authorInfoYAML of
              Right ai' -> ai'
              Left exc -> error $ prettyPrintParseException exc
      ai `shouldBe` authorInfoRec
    it "translates correctly into YAML, tested by re-parsing" $ do
      let yaml = encode authorInfoRec
      let Right ai = decodeEither' yaml
      ai `shouldBe` authorInfoRec

authorInfoYAML :: ByteString
authorInfoYAML =
  fromString
    [i|authorName: Bob Jones
authorEmail: bob@example.com
githubName: bob
|]

authorInfoRec :: AuthorInfo
authorInfoRec =
  AuthorInfo
  { authorName = "Bob Jones"
  , authorEmail = "bob@example.com"
  , githubName = "bob"
  }
