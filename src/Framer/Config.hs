module Framer.Config where

import qualified Data.Set as S

data TestType
  = Hedgehog
  | Hspec
  | HUnit
  | QuickCheck
  | SmallCheck
  | Tasty
  deriving (Bounded, Enum, Eq, Ord, Show)

data Config = Config
  { authorName :: String
  , thisYear :: String
  , projectName :: String
  , githubName :: String
  , authorEmail :: String
  , tastyDiscoverTests :: Bool
  , tastyTestTypes :: S.Set TestType
  } deriving (Show)

config :: Config
config =
  Config
  { authorName = "Eric Nedervold"
  , thisYear = "2019"
  , projectName = "sample"
  , githubName = "nedervold"
  , authorEmail = "nedervoldsoftware@gmail.com"
  , tastyDiscoverTests = True
  , tastyTestTypes = S.singleton Hedgehog
  }
