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

-- | Type proxy for 'ArrayException'.
arrayException :: Proxy ArrayException
arrayException = Proxy

-- | Type proxy for 'AssertionFailed'.
assertionFailed :: Proxy AssertionFailed
assertionFailed = Proxy

-- | Type proxy for 'AsyncException'.
asyncException :: Proxy AsyncException
asyncException = Proxy

-- | Type proxy for 'BlockedIndefinitelyOnMVar'.
blockedIndefinitelyOnMVar :: Proxy BlockedIndefinitelyOnMVar
blockedIndefinitelyOnMVar = Proxy

-- | Type proxy for 'BlockedIndefinitelyOnSTM'.
blockedIndefinitelyOnSTM :: Proxy BlockedIndefinitelyOnSTM
blockedIndefinitelyOnSTM = Proxy

-- | Type proxy for 'Deadlock'.
deadlock :: Proxy Deadlock
deadlock = Proxy

-- | Type proxy for 'ErrorCall'.
errorCall :: Proxy ErrorCall
errorCall = Proxy

-- | Type proxy for 'IOException'.
ioException :: Proxy IOException
ioException = Proxy

-- | Type proxy for 'NestedAtomically'.
nestedAtomically :: Proxy NestedAtomically
nestedAtomically = Proxy

-- | Type proxy for 'NoMethodError'.
noMethodError :: Proxy NoMethodError
noMethodError = Proxy

-- | Type proxy for 'NonTermination'.
nonTermination :: Proxy NonTermination
nonTermination = Proxy

-- | Type proxy for 'PatternMatchFail'.
patternMatchFail :: Proxy PatternMatchFail
patternMatchFail = Proxy

-- | Type proxy for 'RecConError'.
recConError :: Proxy RecConError
recConError = Proxy

-- | Type proxy for 'RecSelError'.
recSelError :: Proxy RecSelError
recSelError = Proxy

-- | Type proxy for 'RecUpdError'.
recUpdError :: Proxy RecUpdError
recUpdError = Proxy

-- | Type proxy for 'SomeAsyncException'.
someAsyncException :: Proxy SomeAsyncException
someAsyncException = Proxy

-- | Type proxy for 'SomeException'.
someException :: Proxy SomeException
someException = Proxy
