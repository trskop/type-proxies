{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Type proxy for Bool data type.
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Type proxy for 'Bool' data type.
module Data.Proxy.Bool (bool)
  where

import Data.Bool (Bool)
import Data.Proxy (Proxy(Proxy))


-- | Type proxy for 'Bool'.
bool :: Proxy Bool
bool = Proxy
