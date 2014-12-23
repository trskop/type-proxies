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
    )
  where

import Data.Functor (Functor)
import Data.Proxy (Proxy(Proxy))


-- | Restrict a type wrapped in a 'Functor' to type of a 'Proxy'.
inFunctorAsProxyTypeOf :: Functor f => f a -> Proxy a -> f a
inFunctorAsProxyTypeOf fa Proxy = fa