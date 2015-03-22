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

import Data.Function ((.))
import Data.Proxy (Proxy(Proxy), asProxyTypeOf)
import Data.Tuple (fst, snd)


-- | Proxy for a pair.
aPair :: Proxy (a, b)
aPair = Proxy

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

-- | Identity on pairs that restrict type of its first element.
firstOf :: Proxy a -> (a, b) -> (a, b)
firstOf p = (`asProxyTypeOf` aPairOf p Proxy)

-- | Identity on pairs that restrict type of its second element.
secondOf :: Proxy b -> (a, b) -> (a, b)
secondOf p = (`asProxyTypeOf` aPairOf Proxy p)

-- | Identity on pairs that restrict type of its both elements.
bothOf :: Proxy a -> (a, a) -> (a, a)
bothOf p = (`asProxyTypeOf` aPairOf p p)

-- | Variant of 'fst' with type restriction on first element of a pair.
fstOf :: Proxy a -> (a, b) -> a
fstOf p = (`asProxyTypeOf` p) . fst

-- | Variant of 'snd' with type restriction on second element of a pair.
sndOf :: Proxy b -> (a, b) -> b
sndOf p = (`asProxyTypeOf` p) . snd
