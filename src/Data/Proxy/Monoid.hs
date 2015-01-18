{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Type proxies for Data.Monoid module.
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Type proxies for "Data.Monoid" module.
module Data.Proxy.Monoid
    ( aMonoid

    -- * All and Any
    , all
    , any

    -- * Dual
    , dualProxyOf
    , dualOf

    -- * Endo
    , endoProxyOf
    , endoOf

    -- * First
    , firstProxyOf
    , firstOf

    -- * Last
    , lastProxyOf
    , lastOf

    -- * Product
    , productProxyOf
    , productOf

    -- * Sum
    , sumProxyOf
    , sumOf
    )
  where

import Data.Maybe (Maybe)
import Data.Monoid
    ( All
    , Any
    , Dual(Dual)
    , Endo(Endo)
    , First(First)
    , Last(Last)
    , Monoid
    , Product(Product)
    , Sum(Sum)
    )
import Data.Proxy (Proxy(Proxy))


aMonoid :: Monoid a => Proxy a
aMonoid = Proxy

-- {{{ All and Any ------------------------------------------------------------

-- | Type proxy for 'All' data type.
all :: Proxy All
all = Proxy

-- | Type proxy for 'Any' data type.
any :: Proxy Any
any = Proxy

-- }}} All and Any ------------------------------------------------------------

-- {{{ Dual -------------------------------------------------------------------

-- | Parameterise Type proxy for 'Any' data type.
dualProxyOf :: Proxy a -> Proxy (Dual a)
dualProxyOf Proxy = Proxy

-- | Constructor of 'Dual' parameterised by a type proxy of a value.
--
-- @
-- 'dualOf' 'aMonoid' :: 'Monoid' a => a -> 'Dual' a
-- ('Dual' .) . 'endoOf' :: 'Proxy' a -> (a -> a) -> 'Dual' ('Endo' a)
-- @
dualOf :: Proxy a -> a -> Dual a
dualOf Proxy = Dual

-- }}} Dual -------------------------------------------------------------------

-- {{{ Endo -------------------------------------------------------------------

endoProxyOf :: Proxy a -> Proxy (Endo a)
endoProxyOf Proxy = Proxy

-- | Constructor of 'Endo' parameterised by a type proxy of a value.
endoOf :: Proxy a -> (a -> a) -> Endo a
endoOf Proxy = Endo

-- }}} Endo -------------------------------------------------------------------

-- {{{ First ------------------------------------------------------------------

firstProxyOf :: Proxy a -> Proxy (First a)
firstProxyOf Proxy = Proxy

-- | Constructor of 'First' parameterised by a type proxy of a value.
firstOf :: Proxy a -> Maybe a -> First a
firstOf Proxy = First

-- }}} First ------------------------------------------------------------------

-- {{{ Last -------------------------------------------------------------------

lastProxyOf :: Proxy a -> Proxy (Last a)
lastProxyOf Proxy = Proxy

-- | Constructor of 'Last' parameterised by a type proxy of a value.
lastOf :: Proxy a -> Maybe a -> Last a
lastOf Proxy = Last

-- }}} Last -------------------------------------------------------------------

-- {{{ Product ----------------------------------------------------------------

productProxyOf :: Proxy a -> Proxy (Product a)
productProxyOf Proxy = Proxy

-- | Constructor of 'Product' parameterised by a type proxy of a value.
--
-- @
-- 'productOf' 'aMonoid' :: 'Monoid' a => a -> 'Product' a
-- @
productOf :: Proxy a -> a -> Product a
productOf Proxy = Product

-- }}} Product ----------------------------------------------------------------

-- {{{ Sum --------------------------------------------------------------------

sumProxyOf :: Proxy a -> Proxy (Sum a)
sumProxyOf Proxy = Proxy

-- | Constructor of 'Sum' parameterised by a type proxy of a value.
--
-- @
-- 'sumOf' 'aMonoid' :: 'Monoid' a => a -> 'Sum' a
-- @
sumOf :: Proxy a -> a -> Sum a
sumOf Proxy = Sum

-- }}} Sum --------------------------------------------------------------------
