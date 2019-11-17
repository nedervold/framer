{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Framer.Config where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Time.Calendar (toGregorian)
import Data.Time.LocalTime
       (LocalTime(..), ZonedTime(..), getZonedTime)
import Data.Yaml
       (FromJSON(..), Parser, ToJSON(..), Value(..), (.=), (.:),
        decodeFileEither, object, prettyPrintParseException, withObject,
        withText)
import System.Directory
       (XdgDirectory(..), doesFileExist, getXdgDirectory)
import System.FilePath ((</>))
import Text.Printf (printf)

data TestType
  = Hedgehog
  | Hspec
  | HUnit
  | QuickCheck
  | SmallCheck
  | Tasty
  deriving (Bounded, Enum, Eq, Ord, Show)

allTestTypes :: [TestType]
allTestTypes = [minBound .. maxBound]

instance FromJSON TestType where
  parseJSON = withText "TestType" f
    where
      f :: Text -> Parser TestType
      f txt =
        case M.lookup txt m of
          Just tt -> pure tt
          Nothing ->
            fail $
            printf
              "Expected one of %s; got %s."
              (show $ map show allTestTypes)
              (show txt)
      m :: M.Map Text TestType
      m = M.fromList [(fromString $ show tt, tt) | tt <- allTestTypes]

instance ToJSON TestType where
  toJSON tt = String $ fromString $ show tt

------------------------------------------------------------
data App = App
  { appName :: String
  , appModuleName :: String
  , isFancy :: Bool
  } deriving (Eq, Show)

instance FromJSON App where
  parseJSON =
    withObject "App" $ \o ->
      App <$> o .: "appName" <*> o .: "appModuleName" <*> o .: "isFancy"

instance ToJSON App where
  toJSON App {..} =
    object
      [ "appName" .= appName
      , "appModuleName" .= appModuleName
      , "isFancy" .= isFancy
      ]

------------------------------------------------------------
data AuthorInfo = AuthorInfo
  { authorName :: String
  , authorEmail :: String
  , githubName :: String
  } deriving (Eq, Show)

instance FromJSON AuthorInfo where
  parseJSON =
    withObject "AuthorInfo" $ \o ->
      AuthorInfo <$> o .: "authorName" <*> o .: "authorEmail" <*>
      o .: "githubName"

instance ToJSON AuthorInfo where
  toJSON AuthorInfo {..} =
    object
      [ "authorName" .= authorName
      , "authorEmail" .= authorEmail
      , "githubName" .= githubName
      ]

defaultAuthorInfo :: AuthorInfo
defaultAuthorInfo =
  AuthorInfo
  { authorName = "Joe Author"
  , githubName = "jauthor"
  , authorEmail = "jauthor@example.com"
  }

getAuthorInfo :: IO AuthorInfo
getAuthorInfo = do
  dir <- getXdgDirectory XdgConfig "framer"
  let configFilePath = dir </> "authorInfo.yaml"
  exists <- doesFileExist configFilePath
  if exists
    then do
      res <- decodeFileEither configFilePath
      case res of
        Right ai -> return ai
        Left exc -> error $ prettyPrintParseException exc
    else return defaultAuthorInfo

------------------------------------------------------------
data ProjectInfo = ProjectInfo
  { projectName :: String
  , apps :: [App]
  , tastyTestTypes :: S.Set TestType
  } deriving (Eq, Show)

instance FromJSON ProjectInfo where
  parseJSON =
    withObject "ProjectInfo" $ \o ->
      ProjectInfo <$> o .: "projectName" <*> o .: "apps" <*>
      o .: "tastyTestTypes"

instance ToJSON ProjectInfo where
  toJSON ProjectInfo {..} =
    object
      [ "projectName" .= projectName
      , "apps" .= apps
      , "tastyTestTypes" .= tastyTestTypes
      ]

------------------------------------------------------------
data Config = Config
  { thisYear :: String
  , authorInfo :: AuthorInfo
  , projectInfo :: ProjectInfo
  } deriving (Show)

needsFancy :: Config -> Bool
needsFancy Config {..} = any isFancy $ apps projectInfo

tastyDiscoverTests :: Config -> Bool
tastyDiscoverTests Config {..} = not $ S.null $ tastyTestTypes projectInfo

getYearAsString :: IO String
getYearAsString = do
  zonedTime <- getZonedTime
  let localTime = zonedTimeToLocalTime zonedTime
  let d = localDay localTime
  let (y, _m, _d) = toGregorian d
  return $ show y

getConfig :: IO Config
getConfig = do
  y <- getYearAsString
  ai <- getAuthorInfo
  return
    Config
    { thisYear = y
    , authorInfo = ai
    , projectInfo =
        ProjectInfo
        { projectName = "sample"
        , apps =
            [App "sample-exe" "Sample" True, App "namuna" "Namuna" False]
        , tastyTestTypes = S.singleton Hedgehog
        }
    }
