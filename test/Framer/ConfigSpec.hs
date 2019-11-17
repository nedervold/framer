{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Framer.ConfigSpec where

import Data.ByteString (ByteString)
import qualified Data.Set as S
import Data.String (IsString(..))
import Data.String.Interpolate (i)
import Data.Yaml
import Framer.Config
import Test.Tasty.Hspec

sureDecode
  :: FromJSON s
  => ByteString -> s
sureDecode yaml =
  case decodeEither' yaml of
    Right v -> v
    Left exc -> error $ prettyPrintParseException exc

roundtripYAML
  :: (FromJSON s, ToJSON s)
  => s -> s
roundtripYAML = sureDecode . encode

spec_config :: Spec
spec_config = do
  describe "TestType" $ do
    it "parses correctly from YAML" $ do
      let tt = sureDecode "HUnit"
      tt `shouldBe` HUnit
    it "translates correctly into YAML; tested by reparsing" $ do
      let tt' = roundtripYAML Hedgehog
      tt' `shouldBe` Hedgehog
  describe "set of TestType" $ do
    let tts = S.fromList [HUnit, Hedgehog]
    it "parses correctly from YAML" $ do
      let yaml = "- HUnit\n- Hedgehog"
      let tts' = sureDecode yaml
      tts' `shouldBe` tts
    it "translates correctly into YAML; tested by reparsing" $ do
      let tts' = roundtripYAML tts
      tts' `shouldBe` tts
  describe "App" $ do
    it "parses correctly from YAML" $ do
      let app = sureDecode appYAML
      app `shouldBe` appRec
    it "translates correctly into YAML; tested by reparsing" $ do
      let app' = roundtripYAML appRec
      app' `shouldBe` appRec
  describe "AuthorInfo" $ do
    it "parses correctly from YAML" $ do
      let ai = sureDecode authorInfoYAML
      ai `shouldBe` authorInfoRec
    it "translates correctly into YAML, tested by reparsing" $ do
      let ai = roundtripYAML authorInfoRec
      ai `shouldBe` authorInfoRec
  describe "ProjectInfo" $ do
    it "parses correctly from YAML" $ do
      let projInfo = sureDecode projectInfoYAML
      projInfo `shouldBe` projectInfoRec
    it "translates correctly into YAML, tested by reparsing" $ do
      let projInfo = roundtripYAML projectInfoRec
      projInfo `shouldBe` projectInfoRec

------------------------------------------------------------
appYAML :: ByteString
appYAML =
  fromString
    [i|appName: munger
appModuleName: Munger
isFancy: False
|]

appRec :: App
appRec = App {appName = "munger", appModuleName = "Munger", isFancy = False}

------------------------------------------------------------
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

------------------------------------------------------------
projectInfoYAML :: ByteString
projectInfoYAML =
  fromString
    [i|
projectName: 'election-interference'
apps:
    - appName: Fezbook
      appModuleName: Fezbook
      isFancy: True
    - appName: mutter
      appModuleName: Mutter
      isFancy: False
tastyTestTypes:
    - Hspec
    - SmallCheck
|]

projectInfoRec :: ProjectInfo
projectInfoRec =
  ProjectInfo
  { projectName = "election-interference"
  , apps =
      [ App {appName = "Fezbook", appModuleName = "Fezbook", isFancy = True}
      , App {appName = "mutter", appModuleName = "Mutter", isFancy = False}
      ]
  , tastyTestTypes = S.fromList [SmallCheck, Hspec]
  }
