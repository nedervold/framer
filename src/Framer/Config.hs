{-# LANGUAGE RecordWildCards #-}

module Framer.Config where

import qualified Data.Set as S
import Data.Time.Calendar (toGregorian)
import Data.Time.LocalTime
       (LocalTime(..), ZonedTime(..), getZonedTime)

data TestType
  = Hedgehog
  | Hspec
  | HUnit
  | QuickCheck
  | SmallCheck
  | Tasty
  deriving (Bounded, Enum, Eq, Ord, Show)

data App = App
  { appName :: String
  , appModuleName :: String
  , isFancy :: Bool
  } deriving (Show)

data AuthorInfo = AuthorInfo
  { authorName :: String
  , githubName :: String
  , authorEmail :: String
  } deriving (Show)

data ProjectInfo = ProjectInfo
  { projectName :: String
  , apps :: [App]
  , tastyDiscoverTests :: Bool
  , tastyTestTypes :: S.Set TestType
  } deriving (Show)

data Config = Config
  { thisYear :: String
  , authorInfo :: AuthorInfo
  , projectInfo :: ProjectInfo
  } deriving (Show)

needsFancy :: Config -> Bool
needsFancy Config {..} = any isFancy $ apps projectInfo

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
  return
    Config
    { thisYear = y
    , authorInfo =
        AuthorInfo
        { authorName = "Eric Nedervold"
        , githubName = "nedervold"
        , authorEmail = "nedervoldsoftware@gmail.com"
        }
    , projectInfo =
        ProjectInfo
        { projectName = "sample"
        , apps =
            [App "sample-exe" "Sample" True, App "namuna" "Namuna" False]
        , tastyDiscoverTests = True
        , tastyTestTypes = S.singleton Hedgehog
        }
    }
