{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_ron_bug_example (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "ron_bug_example"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Demonstrates a bug in RON RGA's applyPatch function"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
