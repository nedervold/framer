{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Framer.Templates.StackYaml where

import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (fromString)
import Data.String.Interpolate (i)
import Framer.Config

-- | TODO Generalize over author name and current year
stackYamlText :: Config -> ByteString
stackYamlText Config {..} =
  fromString
    [i|# This file was automatically generated by 'stack init'
#
# Some commonly used options have been documented as comments in this file.
# For advanced use and comprehensive documentation of the format, please see:
# https://docs.haskellstack.org/en/stable/yaml_configuration/

# Resolver to choose a 'specific' stackage snapshot or a compiler version.
# A snapshot resolver dictates the compiler version and the set of packages
# to be used for project dependencies. For example:
#
# resolver: lts-3.5
# resolver: nightly-2015-09-21
# resolver: ghc-7.10.2
#
# The location of a snapshot can be provided as a file or url. Stack assumes
# a snapshot provided as a file might change, whereas a url resource does not.
#
# resolver: ./custom-snapshot.yaml
# resolver: https://example.com/snapshots/2018-01-01.yaml
resolver: lts-14.14

# User packages to be built.
# Various formats can be used as shown in the example below.
#
# packages:
# - some-directory
# - https://example.com/foo/bar/baz-0.0.2.tar.gz
#   subdirs:
#   - auto-update
#   - wai
packages:
- .
# Dependency packages to be pulled from upstream that are not in the resolver.
# These entries can reference officially published versions as well as
# forks / in-progress versions pinned to a git hash. For example:
#
# extra-deps:
# - acme-missiles-0.3
# - git: https://github.com/commercialhaskell/stack.git
#   commit: e7b331f14bcffb8367cd58fbfc8b40ec7642100a
#
# extra-deps: []

# Override default flag values for local packages and extra-deps
# flags: {}

# Extra package databases containing global packages
# extra-package-dbs: []

# Control whether we use the GHC we find on the path
# system-ghc: true
#
# Require a specific version of stack, using version ranges
# require-stack-version: -any # Default
# require-stack-version: ">=2.1"
#
# Override the architecture used by stack, especially useful on Windows
# arch: i386
# arch: x86_64
#
# Extra directories used by stack for building
# extra-include-dirs: [/path/to/dir]
# extra-lib-dirs: [/path/to/dir]
#
# Allow a newer minor version of GHC than the snapshot specifies
# compiler-check: newer-minor

|]