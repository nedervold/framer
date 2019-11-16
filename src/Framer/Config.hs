module Framer.Config where

data Config = Config
  { authorName :: String
  , thisYear :: String
  , projectName :: String
  , githubName :: String
  , authorEmail :: String
  , tastyDiscoverTests :: Bool
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
  }
