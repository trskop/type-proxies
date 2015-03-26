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
    , anAll
    , anAny

    -- * Dual
    , aDual
    , aDualOf
    , dualOf

    -- * Endo
    , anEndo
    , anEndoOf
    , endoOf
    , appEndoTo
    , runEndoOn

    -- * First
    , aFirst
    , aFirstOf
    , firstOf

    -- * Last
    , aLast
    , aLastOf
    , lastOf

    -- * Product
    , aProduct
    , aProductOf
    , productOf

    -- * Sum
    , aSum
    , aSumOf
    , sumOf
    )
  where

import Data.Function (flip)
import Data.Maybe (Maybe)
import Data.Monoid
    ( All
    , Any
    , Dual(Dual)
    , Endo(Endo, appEndo)
    , First(First)
    , Last(Last)
    , Monoid
    , Product(Product)
    , Sum(Sum)
    )
import Data.Proxy (Proxy(Proxy))


aMonoid :: Monoid a => Proxy a
aMonoid = Proxy
{-# INLINE aMonoid #-}

-- {{{ All and Any ------------------------------------------------------------

-- | Type proxy for 'All' data type.
anAll :: Proxy All
anAll = Proxy
{-# INLINE anAll #-}

-- | Type proxy for 'Any' data type.
anAny :: Proxy Any
anAny = Proxy
{-# INLINE anAny #-}

-- }}} All and Any ------------------------------------------------------------

-- {{{ Dual -------------------------------------------------------------------

-- | Type proxy for 'Dual' data type.
aDual :: Proxy (Dual a)
aDual = Proxy
{-# INLINE aDual #-}

-- | Restricted type proxy for 'Dual' data type.
aDualOf :: Proxy a -> Proxy (Dual a)
aDualOf Proxy = Proxy
{-# INLINE aDualOf #-}

-- | Constructor of 'Dual' parameterised by a type proxy of a value.
--
-- @
-- 'dualOf' 'aMonoid' :: 'Monoid' a => a -> 'Dual' a
-- ('Dual' .) . 'endoOf' :: 'Proxy' a -> (a -> a) -> 'Dual' ('Endo' a)
-- @
dualOf :: Proxy a -> a -> Dual a
dualOf Proxy = Dual
{-# INLINE dualOf #-}

-- }}} Dual -------------------------------------------------------------------

-- {{{ Endo -------------------------------------------------------------------

-- | Proxy for 'Endo' data type.
anEndo :: Proxy (Endo a)
anEndo = Proxy
{-# INLINE anEndo #-}

-- | Proxy for 'Endo' data type that restrict the type endo operates on.
anEndoOf :: Proxy a -> Proxy (Endo a)
anEndoOf Proxy = Proxy
{-# INLINE anEndoOf #-}

-- | Constructor of 'Endo' parameterised by a type proxy of a value.
endoOf :: Proxy a -> (a -> a) -> Endo a
endoOf Proxy = Endo
{-# INLINE endoOf #-}

-- | Type restricted version of 'appEndo'.
appEndoTo :: Proxy a -> Endo a -> a -> a
appEndoTo Proxy = appEndo
{-# INLINE appEndoTo #-}

-- | Type restricted version of flipped 'appEndo'.
runEndoOn :: Proxy a -> a -> Endo a -> a
runEndoOn Proxy = flip appEndo
{-# INLINE runEndoOn #-}

-- }}} Endo -------------------------------------------------------------------

-- {{{ First ------------------------------------------------------------------

aFirst :: Proxy (First a)
aFirst = Proxy
{-# INLINE aFirst #-}

aFirstOf :: Proxy a -> Proxy (First a)
aFirstOf Proxy = Proxy
{-# INLINE aFirstOf #-}

-- | Constructor of 'First' parameterised by a type proxy of a value.
firstOf :: Proxy a -> Maybe a -> First a
firstOf Proxy = First
{-# INLINE firstOf #-}

-- }}} First ------------------------------------------------------------------

-- {{{ Last -------------------------------------------------------------------

aLast :: Proxy (Last a)
aLast = Proxy
{-# INLINE aLast #-}

aLastOf :: Proxy a -> Proxy (Last a)
aLastOf Proxy = Proxy
{-# INLINE aLastOf #-}

-- | Constructor of 'Last' parameterised by a type proxy of a value.
lastOf :: Proxy a -> Maybe a -> Last a
lastOf Proxy = Last
{-# INLINE lastOf #-}

-- }}} Last -------------------------------------------------------------------

-- {{{ Product ----------------------------------------------------------------

aProduct :: Proxy (Product a)
aProduct = Proxy
{-# INLINE aProduct #-}

aProductOf :: Proxy a -> Proxy (Product a)
aProductOf Proxy = Proxy
{-# INLINE aProductOf #-}

-- | Constructor of 'Product' parameterised by a type proxy of a value.
--
-- @
-- 'productOf' 'aMonoid' :: 'Monoid' a => a -> 'Product' a
-- @
productOf :: Proxy a -> a -> Product a
productOf Proxy = Product
{-# INLINE productOf #-}

-- }}} Product ----------------------------------------------------------------

-- {{{ Sum --------------------------------------------------------------------

aSum :: Proxy (Sum a)
aSum = Proxy
{-# INLINE aSum #-}

aSumOf :: Proxy a -> Proxy (Sum a)
aSumOf Proxy = Proxy
{-# INLINE aSumOf #-}

-- | Constructor of 'Sum' parameterised by a type proxy of a value.
--
-- @
-- 'sumOf' 'aMonoid' :: 'Monoid' a => a -> 'Sum' a
-- @
sumOf :: Proxy a -> a -> Sum a
sumOf Proxy = Sum
{-# INLINE sumOf #-}

-- }}} Sum --------------------------------------------------------------------
