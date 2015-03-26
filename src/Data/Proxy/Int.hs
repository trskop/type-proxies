{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Concrete proxies for types from Data.Int
-- Copyright:    (c) 2014 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Concrete proxies for types from "Data.Int".
module Data.Proxy.Int
    (
      int
    , int8
    , int16
    , int32
    , int64
    )
  where

import Data.Int (Int, Int8, Int16, Int32, Int64)
import Data.Proxy (Proxy(Proxy))


-- | Type proxy for 'Int'.
int :: Proxy Int
int = Proxy
{-# INLINE int #-}

-- | Type proxy for 'Int8'.
int8 :: Proxy Int8
int8 = Proxy
{-# INLINE int8 #-}

-- | Type proxy for 'Int16'.
int16 :: Proxy Int16
int16 = Proxy
{-# INLINE int16 #-}

-- | Type proxy for 'Int32'.
int32 :: Proxy Int32
int32 = Proxy
{-# INLINE int32 #-}

-- | Type proxy for 'Int64'.
int64 :: Proxy Int64
int64 = Proxy
{-# INLINE int64 #-}
