{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Framer.Config where

import qualified Data.Set as S
import Data.Time.Calendar (toGregorian)
import Data.Time.LocalTime
       (LocalTime(..), ZonedTime(..), getZonedTime)
import Data.Yaml
       (FromJSON(..), ToJSON(..), (.=), (.:), decodeFileEither, object,
        prettyPrintParseException, withObject)
import System.Directory
       (XdgDirectory(..), doesFileExist, getXdgDirectory)
import System.FilePath ((</>))

data TestType
  = Hedgehog
  | Hspec
  | HUnit
  | QuickCheck
  | SmallCheck
  | Tasty
  deriving (Bounded, Enum, Eq, Ord, Show)

------------------------------------------------------------
data App = App
  { appName :: String
  , appModuleName :: String
  , isFancy :: Bool
  } deriving (Show)

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
  let configFilePath :: FilePath = dir </> "authorInfo.yaml"
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
  } deriving (Show)

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
