{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Proxy helper functions for Functor
-- Copyright:    (c) 2014 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Proxy helper functions for 'Functor'.
module Data.Proxy.Functor
    (
      inFunctorAsProxyTypeOf
    , aFunctor
    , aFunctorOf
    )
  where

import Data.Functor (Functor)
import Data.Proxy (Proxy(Proxy))


-- | Restrict a type wrapped in a 'Functor' to type of a 'Proxy'.
inFunctorAsProxyTypeOf :: Functor f => f a -> Proxy a -> f a
inFunctorAsProxyTypeOf fa Proxy = fa

-- | Type proxy for a 'Functor'. This can be used to force functor restriction
-- on something.
--
-- @
-- \\x -> x `Data.Proxy.asProxyTypeOf` 'functorProxy'
--     :: 'Functor' f => f a -> f a
-- @
aFunctor :: Functor f => Proxy (f a)
aFunctor = Proxy

-- | Type proxy for a 'Functor' where value wrapped inside is restricted by
-- its own type proxy.
--
-- @
-- 'functorProxyOf' 'Data.Proxy.Int.int'
--     :: 'Functor' f => 'Proxy' (f 'Data.Int.Int')
-- @
aFunctorOf :: Functor f => Proxy a -> Proxy (f a)
aFunctorOf Proxy = Proxy
