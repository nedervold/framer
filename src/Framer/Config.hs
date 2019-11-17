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

data Config = Config
  { thisYear :: String
  , authorName :: String
  , projectName :: String
  , githubName :: String
  , authorEmail :: String
  , tastyDiscoverTests :: Bool
  , tastyTestTypes :: S.Set TestType
  , apps :: [App]
  } deriving (Show)

needsFancy :: Config -> Bool
needsFancy Config {..} = any isFancy apps

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
    , authorName = "Eric Nedervold"
    , projectName = "sample"
    , githubName = "nedervold"
    , authorEmail = "nedervoldsoftware@gmail.com"
    , tastyDiscoverTests = True
    , tastyTestTypes = S.singleton Hedgehog
    , apps = [App "sample-exe" "Sample" True, App "namuna" "Namuna" False]
    }
