{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Type proxy for Char data type.
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Type proxy for 'Char' data type.
module Data.Proxy.Char (char)
  where

import Data.Char (Char)
import Data.Proxy (Proxy(Proxy))


-- | Type proxy for 'Char'.
char :: Proxy Char
char = Proxy
{-# INLINE char #-}
