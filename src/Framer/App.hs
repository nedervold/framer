{-# LANGUAGE OverloadedStrings #-}

module Framer.App
  ( main
  ) where

import qualified Data.ByteString as BS
import Data.FSEntries.IO (writeFSEntriesToFS)
import Data.FSEntries.Types
import Framer.ChangeLog (changeLogText)
import Framer.Config
import Framer.GitIgnore (gitIgnoreText)
import Framer.Lib (libText)
import Framer.License (licenseText)
import Framer.Main (mainText)
import Framer.PackageYaml (packageYamlText)
import Framer.ReadMe (readMeText)
import Framer.Setup (setupText)
import Framer.Spec (specText)
import Framer.StackYaml (stackYamlText)
import System.Directory (createDirectory, makeAbsolute)
import System.Environment (getArgs)
import Text.Printf (printf)

main :: IO ()
main = do
  [targetDir] <- getArgs
  putStrLn "Hello, from Framer!"
  targetDir' <- makeAbsolute targetDir
  let entries =
        mkFSEntries
          [ mkFile ".gitignore" $ gitIgnoreText config
          , mkFile "ChangeLog.md" $ changeLogText config
          , mkFile "package.yaml" $ packageYamlText config
          , mkFile "Setup.hs" $ setupText config
          , mkFile "stack.yaml" $ stackYamlText config
          , mkFile "LICENSE" $ licenseText config
          , mkFile "README.md" $ readMeText config
          , mkDir "app" () [mkFile "Main.hs" $ mainText config]
          , mkDir "src" () [mkFile "Lib.hs" $ libText config]
          , mkDir "test" () [mkFile "Spec.hs" $ specText config]
          ]
  createDirectory targetDir'
  writeFSEntriesToFS targetDir' entries
  putStrLn $ printf "I wrote the project into %s." targetDir'
