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
    , thatForgottenReturn
    )
  where

import Data.Function (const, flip, id)
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
idOf :: Proxy a => a -> a
idOf = flip asProxyTypeOf

-- | Type restricted variant of 'const'.
--
-- @
-- 'forget' 'Data.Proxy.Exception.ioException' 'Data.Maybe.Nothing'
--     :: 'Control.Exception.IOException' -> 'Data.Maybe.Maybe' a
-- @
forget :: Proxy b -> a -> b -> a
forget Proxy = const

-- | Type restricted version of @'flip' 'const'@.
--
-- @
-- 'Data.Proxy.Exception.ioException' `thatForgottenReturn` 'Data.Maybe.Nothing'
--     :: 'Control.Exception.IOException' -> 'Data.Maybe.Maybe' a
-- @
thatForgottenReturn :: Proxy a => a -> b -> b
thatForgottenReturn Proxy = flip const
