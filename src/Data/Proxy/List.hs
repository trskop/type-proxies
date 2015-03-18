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
    )
  where

import Data.Maybe (Maybe(Just, Nothing))
import Data.Proxy (Proxy(Proxy), asProxyTypeOf)


aListOf :: Proxy a -> Proxy [a]
aListOf Proxy = Proxy

aList :: Proxy [a]
aList = aListOf Proxy

listOf :: Proxy a -> [a] -> [a]
listOf p = (`asProxyTypeOf` aListOf p)

consOf :: Proxy a -> a -> [a] -> [a]
consOf p x xs = x : xs `asProxyTypeOf` aListOf p

unconsOf :: Proxy a -> [a] -> Maybe (a, [a])
unconsOf p xs = uncons (xs `asProxyTypeOf` aListOf p)
  where
    uncons []       = Nothing
    uncons (y : ys) = Just (y, ys)
