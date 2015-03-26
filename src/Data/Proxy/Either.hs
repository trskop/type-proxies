{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Type proxy for Either data type.
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Type proxy for 'Either' data type.
module Data.Proxy.Either
    (
    -- * Type Proxies For Either
      anEither
    , anEitherOf
    , aLeftOf
    , aRightOf

    -- * Type Restricted Constructors
    , leftOf
    , rightOf
    )
  where

import Data.Either (Either(Left, Right))
import Data.Function ((.))
import Data.Proxy (Proxy(Proxy), asProxyTypeOf)


-- | Type proxy for 'Either'.
anEither :: Proxy (Either a b)
anEither = anEitherOf Proxy Proxy
{-# INLINE anEither #-}

-- | Type proxy for 'Either' that restricts both of its type arguments.
anEitherOf :: Proxy a -> Proxy b -> Proxy (Either a b)
anEitherOf Proxy Proxy = Proxy
{-# INLINE anEitherOf #-}

-- | Type proxy for 'Either' that restricts only its left type arguments.
aLeftOf :: Proxy a -> Proxy (Either a b)
aLeftOf = (`anEitherOf` Proxy)
{-# INLINE aLeftOf #-}

-- | Type proxy for 'Either' that restricts only its right type arguments.
aRightOf :: Proxy b -> Proxy (Either a b)
aRightOf = anEitherOf Proxy
{-# INLINE aRightOf #-}

-- | Restricted variant of 'Left' constructor of 'Either'.
leftOf :: Proxy a -> a -> Either a b
leftOf p = (`asProxyTypeOf` aLeftOf p) . Left
{-# INLINE leftOf #-}

-- | Restricted variant of 'Right' constructor of 'Either'.
rightOf :: Proxy b -> b -> Either a b
rightOf p =  (`asProxyTypeOf` aRightOf p) . Right
{-# INLINE rightOf #-}
