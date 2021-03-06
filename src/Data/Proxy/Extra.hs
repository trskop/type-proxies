{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
-- |
-- Module:       $HEADER$
-- Description:  Extending Data.Proxy module.
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Extending "Data.Proxy" module.
module Data.Proxy.Extra
    (
    -- * Proxy
      module Data.Proxy

    -- ** Constructors
    , aProxy1
    , aProxy2
    , aProxy3
    , aProxy4
    , aProxy5

    , aProxy1Of
    , aProxy2Of
    , aProxy3Of
    , aProxy4Of
    , aProxy5Of

    -- ** Combinators
    , is
    , also
    , restrictedTo
    )
  where

import Data.Function (flip)
import Data.Proxy


aProxy1 :: Proxy (t a)
aProxy1 = Proxy
{-# INLINE aProxy1 #-}

aProxy2 :: Proxy (t a b)
aProxy2 = Proxy
{-# INLINE aProxy2 #-}

aProxy3 :: Proxy (t a b c)
aProxy3 = Proxy
{-# INLINE aProxy3 #-}

aProxy4 :: Proxy (t a b c d)
aProxy4 = Proxy
{-# INLINE aProxy4 #-}

aProxy5 :: Proxy (t a b c d e)
aProxy5 = Proxy
{-# INLINE aProxy5 #-}

aProxy1Of :: Proxy a -> Proxy (t a)
aProxy1Of Proxy = aProxy1
{-# INLINE aProxy1Of #-}

aProxy2Of :: Proxy a -> Proxy b -> Proxy (t a b)
aProxy2Of Proxy Proxy = aProxy2
{-# INLINE aProxy2Of #-}

aProxy3Of :: Proxy a -> Proxy b -> Proxy c -> Proxy (t a b c)
aProxy3Of Proxy Proxy Proxy = aProxy3
{-# INLINE aProxy3Of #-}

aProxy4Of :: Proxy a -> Proxy b -> Proxy c -> Proxy d -> Proxy (t a b c d)
aProxy4Of Proxy Proxy Proxy Proxy = aProxy4
{-# INLINE aProxy4Of #-}

aProxy5Of
    :: Proxy a -> Proxy b -> Proxy c -> Proxy d -> Proxy e
    -> Proxy (t a b c d e)
aProxy5Of Proxy Proxy Proxy Proxy Proxy = aProxy5
{-# INLINE aProxy5Of #-}

-- | Alias for 'asProxyTypeOf'.
--
-- > map show . (`is` 'Data.Proxy.List.aListOf' 'Data.Proxy.String.aString')
-- >     :: ('Data.String.IsString' a, Show a) => [a] -> [String]
--
is :: a -> Proxy a -> a
is = asProxyTypeOf
{-# INLINE is #-}

-- | Flipped version of 'asProxyTypeOf'
restrictedTo :: Proxy a -> a -> a
restrictedTo = flip asProxyTypeOf
{-# INLINE restrictedTo #-}

-- |
--
-- > 'aProxy1Of' 'Data.Proxy.List.word8' `also` 'Data.Proxy.List.aList'
-- >     :: 'Proxy' ['GHC.Word.Word8']
also :: Proxy a -> Proxy a -> Proxy a
also Proxy Proxy = Proxy
{-# INLINE also #-}
