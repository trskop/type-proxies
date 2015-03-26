{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Concrete proxies for types from Exception instances.
-- Copyright:    (c) 2015, Peter Trško
-- License:      BSD3
--
-- Maintainer:   peter.trsko@gmail.com
-- Stability:    experimental
-- Portability:  NoImplicitPrelude
--
-- Concrete proxies for types from 'Control.Exception.Exception' instances.
module Data.Proxy.Exception
    ( arithException
    , arrayException
    , assertionFailed
    , asyncException
    , blockedIndefinitelyOnMVar
    , blockedIndefinitelyOnSTM
    , deadlock
    , errorCall
    , ioException
    , nestedAtomically
    , noMethodError
    , nonTermination
    , patternMatchFail
    , recConError
    , recSelError
    , recUpdError
    , someAsyncException
    , someException

    -- * Reexported from other modules
    --
    -- | These are also instances of 'Control.Exception.Exception', but they
    -- are primarily used for other purposes then exceptions.
    , exitCode
    , dynamic
    )
  where

import Control.Exception
    ( ArithException
    , ArrayException
    , AssertionFailed
    , AsyncException
    , BlockedIndefinitelyOnMVar
    , BlockedIndefinitelyOnSTM
    , Deadlock
    , ErrorCall
    , IOException
    , NestedAtomically
    , NoMethodError
    , NonTermination
    , PatternMatchFail
    , RecConError
    , RecSelError
    , RecUpdError
    , SomeAsyncException
    , SomeException
    )
import Data.Proxy (Proxy(Proxy))

import Data.Proxy.Dynamic (dynamic)
import Data.Proxy.ExitCode (exitCode)


-- | Type proxy for 'ArithException'.
arithException :: Proxy ArithException
arithException = Proxy
{-# INLINE arithException #-}

-- | Type proxy for 'ArrayException'.
arrayException :: Proxy ArrayException
arrayException = Proxy
{-# INLINE arrayException #-}

-- | Type proxy for 'AssertionFailed'.
assertionFailed :: Proxy AssertionFailed
assertionFailed = Proxy
{-# INLINE assertionFailed #-}

-- | Type proxy for 'AsyncException'.
asyncException :: Proxy AsyncException
asyncException = Proxy
{-# INLINE asyncException #-}

-- | Type proxy for 'BlockedIndefinitelyOnMVar'.
blockedIndefinitelyOnMVar :: Proxy BlockedIndefinitelyOnMVar
blockedIndefinitelyOnMVar = Proxy
{-# INLINE blockedIndefinitelyOnMVar #-}

-- | Type proxy for 'BlockedIndefinitelyOnSTM'.
blockedIndefinitelyOnSTM :: Proxy BlockedIndefinitelyOnSTM
blockedIndefinitelyOnSTM = Proxy
{-# INLINE blockedIndefinitelyOnSTM #-}

-- | Type proxy for 'Deadlock'.
deadlock :: Proxy Deadlock
deadlock = Proxy
{-# INLINE deadlock #-}

-- | Type proxy for 'ErrorCall'.
errorCall :: Proxy ErrorCall
errorCall = Proxy
{-# INLINE errorCall #-}

-- | Type proxy for 'IOException'.
ioException :: Proxy IOException
ioException = Proxy
{-# INLINE ioException #-}

-- | Type proxy for 'NestedAtomically'.
nestedAtomically :: Proxy NestedAtomically
nestedAtomically = Proxy
{-# INLINE nestedAtomically #-}

-- | Type proxy for 'NoMethodError'.
noMethodError :: Proxy NoMethodError
noMethodError = Proxy
{-# INLINE noMethodError #-}

-- | Type proxy for 'NonTermination'.
nonTermination :: Proxy NonTermination
nonTermination = Proxy
{-# INLINE nonTermination #-}

-- | Type proxy for 'PatternMatchFail'.
patternMatchFail :: Proxy PatternMatchFail
patternMatchFail = Proxy
{-# INLINE patternMatchFail #-}

-- | Type proxy for 'RecConError'.
recConError :: Proxy RecConError
recConError = Proxy
{-# INLINE recConError #-}

-- | Type proxy for 'RecSelError'.
recSelError :: Proxy RecSelError
recSelError = Proxy
{-# INLINE recSelError #-}

-- | Type proxy for 'RecUpdError'.
recUpdError :: Proxy RecUpdError
recUpdError = Proxy
{-# INLINE recUpdError #-}

-- | Type proxy for 'SomeAsyncException'.
someAsyncException :: Proxy SomeAsyncException
someAsyncException = Proxy
{-# INLINE someAsyncException #-}

-- | Type proxy for 'SomeException'.
someException :: Proxy SomeException
someException = Proxy
{-# INLINE someException #-}
