{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Framer.App
  ( main
  ) where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)
import Data.FSEntries.IO (writeFSEntriesToFS)
import Data.FSEntries.Types
import Data.String.Interpolate (i)
import Framer.ChangeLog (changeLogText)
import Framer.Config
import Framer.GitIgnore (gitIgnoreText)
import Framer.License (licenseText)
import Framer.Makefile (makefileText)
import Framer.PackageYaml (packageYamlText)
import Framer.ReadMe (readMeText)
import Framer.Setup (setupText)
import Framer.Spec (specText)
import Framer.StackYaml (stackYamlText)
import System.Directory
       (createDirectory, doesDirectoryExist, makeAbsolute)
import System.Environment (getArgs)
import Text.Printf (printf)

main :: IO ()
main = do
  [targetDir] <- getArgs
  putStrLn "Hello, from Framer!"
  targetDir' <- makeAbsolute targetDir
  let entries =
        mkFSEntries
          [ mkFile ".gitignore" $ gitIgnoreText hcConfig
          , mkFile "ChangeLog.md" $ changeLogText hcConfig
          , mkFile "Makefile" $ makefileText hcConfig
          , mkFile "package.yaml" $ packageYamlText hcConfig
          , mkFile "Setup.hs" $ setupText hcConfig
          , mkFile "stack.yaml" $ stackYamlText hcConfig
          , mkFile "LICENSE" $ licenseText hcConfig
          , mkFile "README.md" $ readMeText hcConfig
          , ("app", Dir () $ mkMains hcConfig)
          , ("src", Dir () $ mkSrc hcConfig)
          , mkDir "test" () [mkFile "Spec.hs" $ specText hcConfig]
          ]
  exists <- doesDirectoryExist targetDir'
  unless exists $ createDirectory targetDir'
  writeFSEntriesToFS targetDir' entries

mkMains :: Config -> FSEntries () ByteString
mkMains Config {..} = mkFSEntries $ map mkMainDir apps
  where
    mkMainDir app@App {..} =
      mkDir appName () [mkFile "Main.hs" $ mkMainSrc app]
    mkMainSrc :: App -> ByteString
    mkMainSrc App {..} =
      fromString
        [i|module Main(main) where

import qualified #{appModuleName}.App

main :: IO ()
main = #{appModuleName}.App.main
|]

mkSrc :: Config -> FSEntries () ByteString
mkSrc Config {..} = mkFSEntries $ map mkAppDir apps
  where
    mkAppDir app@App {..} =
      mkDir (printf "%s" appModuleName) () [mkFile "App.hs" $ mkAppSrc app]
    mkAppSrc :: App -> ByteString
    mkAppSrc App {..} =
      fromString
        [i|module #{appModuleName}.App(main) where

main :: IO ()
main = putStrLn "Hello, from #{appName}!"
|]
