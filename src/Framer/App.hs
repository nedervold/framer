{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Framer.App
  ( main
  ) where

import Control.Monad.Identity
import Data.ByteString (ByteString)
import Data.FSEntries.Forest (drawFSEntries)
import Data.FSEntries.IO (writeFSEntriesToFS)
import Data.FSEntries.Types
import Data.FSEntries.Zip (interleave)
import Data.List (intersperse)
import Data.String (IsString(..))
import Data.String.Interpolate (i)
import Data.Validation (Validation(..))
import Framer.Config
import Framer.Templates.ChangeLog (changeLogText)
import Framer.Templates.GitIgnore (gitIgnoreText)
import Framer.Templates.License (licenseText)
import Framer.Templates.Makefile (makefileText)
import Framer.Templates.PackageYaml (packageYamlText)
import Framer.Templates.ReadMe (readMeText)
import Framer.Templates.Setup (setupText)
import Framer.Templates.Spec (specText)
import Framer.Templates.StackYaml (stackYamlText)
import System.Directory (createDirectory, doesDirectoryExist, makeAbsolute)
import System.Environment (getArgs)

main :: IO ()
main = do
  [targetDir] <- getArgs
  putStrLn "Hello, from Framer!"
  targetDir' <- makeAbsolute targetDir
  exists <- doesDirectoryExist targetDir'
  unless exists $ createDirectory targetDir'
  hcConfig <- getConfig
  let projectEntries =
        mkFSEntries
          [ mkFile ".gitignore" $ gitIgnoreText hcConfig
          , mkFile "ChangeLog.md" $ changeLogText hcConfig
          , mkFile "Makefile" $ makefileText hcConfig
          , mkFile "package.yaml" $ packageYamlText hcConfig
          , mkFile "Setup.hs" $ setupText hcConfig
          , mkFile "stack.yaml" $ stackYamlText hcConfig
          , mkFile "LICENSE" $ licenseText hcConfig
          , mkFile "README.md" $ readMeText hcConfig
          ]
  let appEntries =
        concatEntries $ map mkAppEntries $ apps $ projectInfo hcConfig
  let testEntries =
        mkFSEntries [mkDir "test" () [mkFile "Spec.hs" $ specText hcConfig]]
  writeFSEntriesToFS targetDir' $
    concatEntries [projectEntries, appEntries, testEntries]

concatEntries :: [FSEntries () ByteString] -> FSEntries () ByteString
concatEntries fss =
  case interleave fss of
    Failure _ -> error errStr
    Success fs -> fs
  where
    errStr = unlines ("concatEntries: could not interleave" : trees)
    toTree :: FSEntries () ByteString -> String
    toTree = drawFSEntries (const "<dir>") (const "<file>")
    trees :: [String]
    trees = intersperse "------------------------------" $ map toTree fss

mkAppEntries :: App -> FSEntries () ByteString
mkAppEntries App {..} =
  mkFSEntries
    [ mkDir "app" () [mkDir appName () [mkFile "Main.hs" mainSrc]]
    , mkDir
        "src"
        ()
        [ mkDir
            (fromString appModuleName)
            ()
            [ mkFile "App.hs" $
              if isFancy
                then fancyAppSrc
                else appSrc
            ]
        ]
    ]
  where
    appSrc =
      fromString
        [i|module #{appModuleName}.App(main) where

main :: IO ()
main = putStrLn "Hello, from #{appName}!"
|]
    fancyAppSrc =
      fromString
        [i|{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module #{appModuleName}.App(main) where

import Options.Applicative
import RIO
import RIO.ByteString.Lazy

data Config = Config
    deriving (Show)

configParser :: ParserInfo Config
configParser = info (helper <*> versionInfo <*> programConfig)
                    (fullDesc <> progDesc progDescStr <> header headerStr)
    where
    progDescStr :: String
    progDescStr = "<here goes a description of #{appName}>"
    headerStr :: String
    headerStr = "<here goes a header for #{appName}>"
    programConfig :: Parser Config
    programConfig = pure Config

    versionInfo :: Parser (c -> c)
    versionInfo = infoOption "0.1.0.0" (short 'v' <> long "version" <> help "Show version")

------------------------------------------------------------
data Env = Env { configuration :: Config }
    deriving (Show)

mkEnv :: Config -> Env
mkEnv = Env

------------------------------------------------------------
main :: IO ()
main = do
    config <- execParser configParser
    let env = mkEnv config
    runRIO env rioMain

------------------------------------------------------------
rioMain :: RIO Env ()
rioMain = putStrLn "Salutations from #{appName}!"
|]
    mainSrc =
      fromString
        [i|module Main(main) where

import qualified #{appModuleName}.App

main :: IO ()
main = #{appModuleName}.App.main
|]
