{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Type proxy for pairs.
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Type proxy for pairs.
module Data.Proxy.Tuple
    ( aPair
    , aPairOf
    , firstOf
    , secondOf
    , bothOf
    , fstOf
    , sndOf
    )
  where

import Data.Function (id)
import Data.Proxy (Proxy(Proxy))
import Data.Tuple (fst, snd)


-- | Proxy for a pair.
aPair :: Proxy (a, b)
aPair = Proxy
{-# INLINE aPair #-}

-- | Proxy for a pair with restriction on both type parameters.
--
-- Restricting only first, or second, or both type parameters:
--
-- @
-- \p -> 'aPairOf' p     'Proxy' :: 'Proxy' a -> 'Proxy' (a, b)
-- \p -> 'aPairOf' 'Proxy' p     :: 'Proxy' b -> 'Proxy' (a, b)
-- \p -> 'aPairOf' p     p     :: 'Proxy' b -> 'Proxy' (a, b)
-- @
aPairOf :: Proxy a -> Proxy b -> Proxy (a, b)
aPairOf Proxy Proxy = Proxy
{-# INLINE aPairOf #-}

-- | Identity on pairs that restrict type of its first element.
firstOf :: Proxy a -> (a, b) -> (a, b)
firstOf Proxy = id -- \p -> (`asProxyTypeOf` aPairOf p Proxy)
{-# INLINE firstOf #-}

-- | Identity on pairs that restrict type of its second element.
secondOf :: Proxy b -> (a, b) -> (a, b)
secondOf Proxy = id -- \p ->`asProxyTypeOf` aPairOf Proxy p)
{-# INLINE secondOf #-}

-- | Identity on pairs that restrict type of its both elements.
bothOf :: Proxy a -> (a, a) -> (a, a)
bothOf Proxy = id -- \p ->`asProxyTypeOf` aPairOf p p)
{-# INLINE bothOf #-}

-- | Variant of 'fst' with type restriction on first element of a pair.
fstOf :: Proxy a -> (a, b) -> a
fstOf Proxy = fst -- \p -> (`asProxyTypeOf` p) . fst
{-# INLINE fstOf #-}

-- | Variant of 'snd' with type restriction on second element of a pair.
sndOf :: Proxy b -> (a, b) -> b
sndOf Proxy = snd -- \p -> (`asProxyTypeOf` p) . snd
{-# INLINE sndOf #-}
