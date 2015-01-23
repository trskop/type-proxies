{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Concrete proxies for Maybe type
-- Copyright:    (c) 2014 Peter Trsko
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Concrete proxies for 'Maybe' type.
module Data.Proxy.Maybe
    (
      inMaybeAsProxyTypeOf
    , aMaybe
    , aMaybeOf

    -- * Restricted Constructors
    , justOf
    , nothingOf

    -- * Catamorphisms for Maybe
    , maybeOf
    , fromMaybeOf
    , catMaybesOf
    )
  where

import Data.Maybe (Maybe(Just, Nothing), catMaybes, maybe, fromMaybe)
import Data.Proxy (Proxy(Proxy))

import Data.Proxy.Functor (inFunctorAsProxyTypeOf)


-- | Restrict a type wrapped in 'Maybe' to type of a 'Proxy'.
--
-- Example:
--
-- @
-- \x -> 'Just' x `inMaybeAsProxyTypeOf` 'Data.Proxy.Word.word8'
--     :: 'Data.Word.Word8' -> 'Maybe' 'Data.Word.Word8'
-- @
--
-- Above example can be simplified by using function 'justOf':
--
-- @
-- 'justOf' 'Data.Proxy.Word.word8'
--     :: 'Data.Word.Word8' -> 'Maybe' 'Data.Word.Word8'
-- @
inMaybeAsProxyTypeOf :: Maybe a -> Proxy a -> Maybe a
inMaybeAsProxyTypeOf = inFunctorAsProxyTypeOf

-- | Type proxy for 'Maybe'. It can be used in situations where one gets a
-- 'Control.Monad.Monad' and wants it to be a 'Maybe' monad.
--
-- @
-- \\x -> return x `Data.Proxy.asProxyTypeOf` 'aMaybe'
--     :: a -> Maybe a
-- @
aMaybe :: Proxy (Maybe a)
aMaybe = Proxy

-- | Type proxy for @'Maybe' a@ where @a@ is restricted by its own
-- type proxy.
--
-- @
-- 'aMaybeOf' 'Data.Proxy.Word.word8'
--     :: 'Proxy' ('Maybe' 'Data.Word.Word8')
-- @
aMaybeOf :: Proxy a -> Proxy (Maybe a)
aMaybeOf Proxy = Proxy

-- | Type restricted variant of function 'maybe'.
--
-- Examples:
--
-- @
-- 'maybeOf' 'Data.Proxy.Word.word'
--     :: b -> ('Data.Word.Word' -> b) -> 'Maybe' 'Data.Word.Word' -> b
-- @
maybeOf :: Proxy a -> b -> (a -> b) -> Maybe a -> b
maybeOf Proxy = maybe

-- | Type restricted variant of data constructor 'Just'.
--
-- Examples:
--
-- @
-- 'justOf' 'Data.Proxy.Int.int8'
--     :: 'Data.Int.Int8' -> 'Maybe' 'Data.Int.Int8'
-- @
justOf :: Proxy a -> a -> Maybe a
justOf Proxy = Just

-- | Type restricted variant of data constructor 'Nothing'.
--
-- @
-- 'nothingOf' 'Data.Proxy.String.string'
--     :: 'Maybe' 'Data.String.String'
-- @
nothingOf :: Proxy a -> Maybe a
nothingOf Proxy = Nothing

-- | Type restricted variant of 'fromMaybe' function.
fromMaybeOf :: Proxy a -> a -> Maybe a -> a
fromMaybeOf Proxy = fromMaybe

-- | Type restricted variant of 'catMaybes' function.
catMaybesOf :: Proxy a -> [Maybe a] -> [a]
catMaybesOf Proxy = catMaybes
