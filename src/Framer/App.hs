{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Framer.App
  ( main
  ) where

import Control.Monad.Identity
import Data.ByteString (ByteString)
import Data.FSEntries.IO (writeFSEntriesToFS)
import Data.FSEntries.Types
import Data.FSEntries.Zip (zipFSEntriesWithA)
import Data.List (foldl')
import Data.String (IsString(..))
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

main :: IO ()
main = do
  [targetDir] <- getArgs
  putStrLn "Hello, from Framer!"
  targetDir' <- makeAbsolute targetDir
  exists <- doesDirectoryExist targetDir'
  unless exists $ createDirectory targetDir'
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
  let appEntries = concatEntries $ map mkAppEntries $ apps hcConfig
  let testEntries =
        mkFSEntries [mkDir "test" () [mkFile "Spec.hs" $ specText hcConfig]]
  writeFSEntriesToFS targetDir' $
    concatEntries [projectEntries, appEntries, testEntries]

concatEntries :: [FSEntries () ByteString] -> FSEntries () ByteString
concatEntries = foldl' appendEntries (mkFSEntries [])
  where
    appendEntries
      :: FSEntries () ByteString
      -> FSEntries () ByteString
      -> FSEntries () ByteString
    appendEntries lhs rhs = runIdentity $ zipFSEntriesWithA merge lhs rhs
        -- | Given matching entries, the right one prevails.
      where
        merge
          :: Maybe (FSEntry () ByteString)
          -> Maybe (FSEntry () ByteString)
          -> Identity (Maybe (FSEntry () ByteString))
        merge Nothing mRhs = pure mRhs
        merge mLhs Nothing = pure mLhs
        merge (Just (Dir () entries)) (Just (Dir () entries')) =
          pure . Dir () <$> zipFSEntriesWithA merge entries entries'
        merge (Just (File _)) f@(Just (File _)) = pure f
        merge (Just (Dir _ _)) (Just (File _)) = error "dir/file conflict"
        merge (Just (File _)) (Just (Dir _ _)) = error "file/dir conflict"

mkAppEntries :: App -> FSEntries () ByteString
mkAppEntries App {..} =
  mkFSEntries
    [ mkDir "app" () [mkDir appName () [mkFile "Main.hs" mainSrc]]
    , mkDir
        "src"
        ()
        [mkDir (fromString appModuleName) () [mkFile "App.hs" appSrc]]
    ]
  where
    appSrc =
      fromString
        [i|module #{appModuleName}.App(main) where

main :: IO ()
main = putStrLn "Hello, from #{appName}!"
|]
    mainSrc =
      fromString
        [i|module Main(main) where

import qualified #{appModuleName}.App

main :: IO ()
main = #{appModuleName}.App.main
|]
