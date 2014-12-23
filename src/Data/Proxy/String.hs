{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Concrete proxies for String type
-- Copyright:    (c) 2014 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Concrete proxies for 'String' type.
module Data.Proxy.String
    (
    -- * String
      string

    -- * Text
    , text
    , strictText
    , lazyText

    -- * Overloaded Strings
    , aString
    , fromStringTo
    )
  where

import Data.Function ((.))
import Data.Proxy (Proxy(Proxy), asProxyTypeOf)
import Data.String (String, IsString(fromString))

import qualified Data.Text as Strict (Text)
import qualified Data.Text.Lazy as Lazy (Text)


string :: Proxy String
string = Proxy

strictText :: Proxy Strict.Text
strictText = Proxy

lazyText :: Proxy Lazy.Text
lazyText = Proxy

-- | Alias for 'strictText'.
text :: Proxy Strict.Text
text = strictText

aString :: IsString s => Proxy s
aString = Proxy

fromStringTo :: IsString s => String -> Proxy s -> s
fromStringTo = asProxyTypeOf . fromString
