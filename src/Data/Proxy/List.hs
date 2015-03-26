{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Type proxies for standard list data type.
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Type proxies for standard list ('[]') data type.
module Data.Proxy.List
    (
      aList
    , aListOf
    , listOf
    , consOf
    , unconsOf
    , headOf
    )
  where

import Data.Maybe (Maybe(Just, Nothing))
import Data.Proxy (Proxy(Proxy), asProxyTypeOf)


-- | Proxy for a list with restricted type of elements.
aListOf :: Proxy a -> Proxy [a]
aListOf Proxy = Proxy
{-# INLINE aListOf #-}

-- | Proxy for a list without any restriction on the type of elements.
aList :: Proxy [a]
aList = aListOf Proxy
{-# INLINE aList #-}

-- | Identity function on lists that puts additional restriction on element
-- type.
listOf :: Proxy a -> [a] -> [a]
listOf p = (`asProxyTypeOf` aListOf p)
{-# INLINE listOf #-}

-- | Type restricted variant of list constructor ':'.
consOf :: Proxy a -> a -> [a] -> [a]
consOf p x xs = x : xs `asProxyTypeOf` aListOf p
{-# INLINE consOf #-}

-- | Splitting list in to its head (first element) and tail (rest of the list
-- without first element). This function also restrict type of list element.
--
-- @
-- 'unconsOf' 'Data.Proxy.Word.word8' [] === Nothing :: Maybe Word8
-- @
unconsOf :: Proxy a -> [a] -> Maybe (a, [a])
unconsOf p xs = uncons (xs `asProxyTypeOf` aListOf p)
  where
    uncons []       = Nothing
    uncons (y : ys) = Just (y, ys)

-- | Save variant of 'Prelude.head' with type restriction on element type.
headOf :: Proxy a -> [a] -> Maybe a
headOf p xs = case unconsOf p xs of
    Nothing     -> Nothing
    Just (x, _) -> Just x
