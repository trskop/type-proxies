{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Type restricted variants of functions from Data.Function
--               module, and more.
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Type restricted variants of functions from "Data.Function" module, and more.
module Data.Proxy.Function
    (
    -- * Type Proxies For Functions
      aFunction
    , aFunction2
    , aFunction3

    -- * Restrict Functions Using Type Proxies
    , resultOf
    , hasResultOf
    , argumentOf
    , hasArgumentOf

    -- * Restricted Versions of Standard Functions
    , idOf
    , forget
    , thatForgotten

    -- * Utilities
    , is
    )
  where

import Data.Function ((.), const, flip, id)
import Data.Proxy (Proxy(Proxy), asProxyTypeOf)


-- | Type proxy for unary function. Note that @b@ may be a function too. All
-- this type proxy says is that it is at least unary function.
aFunction :: Proxy (a -> b)
aFunction = Proxy

-- | Type proxy for binary function. Note that @b@ may be a function too. All
-- this type proxy says is that it is at least binary function.
aFunction2 :: Proxy (a -> b -> c)
aFunction2 = Proxy

-- | Type proxy for ternary function. Note that @b@ may be a function too. All
-- this type proxy says is that it is at least ternary function.
aFunction3 :: Proxy (a -> b -> c -> d)
aFunction3 = Proxy

-- | Restrict type of result of a function. Flipped version of 'resultOf'.
--
-- @
-- \\f -> f `hasResultOf` 'Data.Proxy.Either.anEitherOf' 'Data.Proxy.String.string' 'Data.Proxy.Int.int'
--     :: (a -> Either String Word64) -> a -> Either String Int
-- @
--
-- @
-- 'Data.Typeable.cast' `hasResultOf` 'Data.Proxy.Maybe.aMaybeOf' 'Data.Proxy.Int.int'
--     :: Typeable a => a -> Maybe Int
-- @
hasResultOf :: (a -> b) -> Proxy b -> a -> b
hasResultOf f Proxy = f

-- | Restrict type of result of a function. Flipped version of 'hasResultOf'.
--
-- @
-- 'resultOf' ('Data.Proxy.Either.anEitherOf' 'Data.Proxy.String.string' 'Data.Proxy.Int.int')
--     :: (a -> Either String Word64) -> a -> Either String Int
-- @
--
-- @
-- 'resultOf' ('Data.Proxy.Maybe.aMaybeOf' 'Data.Proxy.Int.int') 'Data.Typeable.cast'
--     :: Typeable a => a -> Maybe Int
-- @
resultOf :: Proxy b -> (a -> b) -> a -> b
resultOf Proxy = id

-- | Restrict type of an argument of a function. Flipped variant of
-- 'hasArgumentOf'.
argumentOf :: Proxy a -> (a -> b) -> a -> b
argumentOf Proxy = id

-- | Restrict type of an argument of a function. Flipped variant of
-- 'argumentOf'.
hasArgumentOf :: (a -> b) -> Proxy a -> a -> b
hasArgumentOf f Proxy = f

-- | Type restricted identity function 'id' defined as:
--
-- @
-- 'idOf' = 'flip' 'asProxyTypeOf'
-- @
--
-- Examples:
--
-- @
-- 'idOf' 'Data.Proxy.Word.word16' :: 'Data.Word.Word16' -> 'Data.Word.Word16'
-- @
idOf :: Proxy a -> a -> a
idOf = flip asProxyTypeOf

-- | Type restricted variant of 'const'.
--
-- @
-- 'forget' 'Data.Proxy.Exception.ioException' (return Nothing)
--     :: 'Control.Exception.IOException' -> IO (Maybe a)
-- @
forget :: Proxy b -> a -> b -> a
forget p = argumentOf p . const

-- | Type restricted version of @'flip' 'const'@.
--
-- @
-- 'Data.Proxy.Exception.ioException' `thatForgotten` return Nothing
--     :: 'Control.Exception.IOException' -> IO (Maybe a)
-- @
thatForgotten :: Proxy a -> a -> b -> b
thatForgotten p = p `argumentOf` flip const

-- | Alias for 'asProxyTypeOf'.
is :: a -> Proxy a -> a
is = asProxyTypeOf
