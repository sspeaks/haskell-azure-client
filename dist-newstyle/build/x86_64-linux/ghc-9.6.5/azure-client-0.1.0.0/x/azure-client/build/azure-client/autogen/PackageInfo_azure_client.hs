{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_azure_client (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "azure_client"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Just a simple program that takes an input file w/ a specific format and provisions azure resources accordingly. The main focus is the http client that will auto-follow AzureAsync headers until finished"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
