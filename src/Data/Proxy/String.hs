{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Concrete proxies for String type
-- Copyright:    (c) 2014, 2015 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Concrete proxies for 'String' type and .
module Data.Proxy.String
    (
    -- * String
      string

    -- * Overloaded Strings
    , aString
    , fromStringTo
    )
  where

import Data.Function ((.))
import Data.Proxy (Proxy(Proxy), asProxyTypeOf)
import Data.String (String, IsString(fromString))


-- | Proxy data type for 'String' (@['Data.Char.Char']@).
string :: Proxy String
string = Proxy
{-# INLINE string #-}

-- | Proxy for a 'IsString' instance.
aString :: IsString s => Proxy s
aString = Proxy
{-# INLINE aString #-}

-- | Restricted version of 'fromString'.
fromStringTo :: IsString s => String -> Proxy s -> s
fromStringTo = asProxyTypeOf . fromString
{-# INLINE fromStringTo #-}
