{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Concrete proxies for types from Data.Word
-- Copyright:    (c) 2014 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Concrete proxies for types from "Data.Word".
module Data.Proxy.Word
    (
      word
    , word8
    , word16
    , word32
    , word64
    )
  where

import Data.Proxy (Proxy(Proxy))
import Data.Word (Word, Word8, Word16, Word32, Word64)


-- | Type proxy for 'Word'.
word :: Proxy Word
word = Proxy

-- | Type proxy for 'Word8'.
word8 :: Proxy Word8
word8 = Proxy

-- | Type proxy for 'Word16'.
word16 :: Proxy Word16
word16 = Proxy

-- | Type proxy for 'Word32'.
word32 :: Proxy Word32
word32 = Proxy

-- | Type proxy for 'Word64'.
word64 :: Proxy Word64
word64 = Proxy
