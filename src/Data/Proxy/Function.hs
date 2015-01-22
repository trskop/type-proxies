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
      idOf
    , forget
    , thatForgotten
    )
  where

import Data.Function (const, flip)
import Data.Proxy (Proxy(Proxy), asProxyTypeOf)


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
forget Proxy = const

-- | Type restricted version of @'flip' 'const'@.
--
-- @
-- 'Data.Proxy.Exception.ioException' `thatForgotten` return Nothing
--     :: 'Control.Exception.IOException' -> IO (Maybe a)
-- @
thatForgotten :: Proxy a -> a -> b -> b
thatForgotten Proxy = flip const
