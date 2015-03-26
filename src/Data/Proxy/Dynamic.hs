{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Type proxy for Dynamic data type.
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Type proxy for 'Dynamic' data type.
module Data.Proxy.Dynamic
    ( dynamic
    , fromDynTo
    , fromDynamicTo
    )
  where

import Data.Dynamic (Dynamic, fromDyn, fromDynamic)
import Data.Maybe (Maybe)
import Data.Proxy (Proxy(Proxy))
import Data.Typeable (Typeable)


-- | Type proxy for 'Dynamic'.
dynamic :: Proxy Dynamic
dynamic = Proxy
{-# INLINE dynamic #-}

-- | Type restricted variant of 'fromDyn'.
fromDynTo :: Typeable a => Proxy a -> Dynamic -> a -> a
fromDynTo Proxy = fromDyn
{-# INLINE fromDynTo #-}

-- | Type restricted variant of 'fromDynamicTo'.
fromDynamicTo :: Typeable a => Proxy a -> Dynamic -> Maybe a
fromDynamicTo Proxy = fromDynamic
{-# INLINE fromDynamicTo #-}
