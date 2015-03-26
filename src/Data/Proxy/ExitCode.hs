{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Type proxy for ExitCode data type.
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Type proxy for 'ExitCode' data type.
module Data.Proxy.ExitCode (exitCode)
  where

import Data.Proxy (Proxy(Proxy))
import System.Exit (ExitCode)


-- | Type proxy for 'ExitCode'.
exitCode :: Proxy ExitCode
exitCode = Proxy
{-# INLINE exitCode #-}
