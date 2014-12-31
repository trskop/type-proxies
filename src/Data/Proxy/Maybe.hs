{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Concrete proxies for Maybe type
-- Copyright:    (c) 2014 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Concrete proxies for 'Maybe' type.
module Data.Proxy.Maybe
    (

    )
  where

import Data.Maybe (Maybe(Just, Nothing), catMaybes, maybe, fromMaybe)
import Data.Proxy (Proxy(Proxy))

import Data.Proxy.Functor (inFunctorAsProxyTypeOf)


-- | Restrict a type wrapped in 'Maybe' to type of a 'Proxy'.
inMaybeAsProxyTypeOf :: Maybe a -> Proxy a -> Maybe a
inMaybeAsProxyTypeOf = inFunctorAsProxyTypeOf

-- | Type restricted variant of function 'maybe'.
maybeOf :: Proxy a -> b -> (a -> b) -> Maybe a -> b
maybeOf Proxy = maybe

-- | Type restricted variant of data constructor 'Just'.
justOf :: Proxy a -> a -> Maybe a
justOf Proxy = Just

-- | Type restricted variant of data constructor 'Nothing'.
nothingOf :: Proxy a -> Maybe a
nothingOf Proxy = Nothing

-- | Type restricted variant of 'fromMaybe' function.
fromMaybeOf :: Proxy a -> a -> Maybe a -> a
fromMaybeOf Proxy = fromMaybe

-- | Type restricted variant of 'catMaybes' function.
catMaybesOf :: Proxy a -> [Maybe a] -> [a]
catMaybesOf Proxy = catMaybes
