{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Type proxy for Dynamic data type.
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Type proxy for 'Dynamic' data type.
module Data.Proxy.Dynamic (dynamic)
  where

import Data.Dynamic (Dynamic)
import Data.Proxy (Proxy(Proxy))


-- | Type proxy for 'Dynamic'.
dynamic :: Proxy Dynamic
dynamic = Proxy
