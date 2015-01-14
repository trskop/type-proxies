{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Concrete proxies for types from Exception instances.
-- Copyright:    (c) 2015 Peter Trsko
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
    , dynamic
    , errorCall
    , exitCode
    , iOException
    , nestedAtomically
    , noMethodError
    , nonTermination
    , patternMatchFail
    , recConError
    , recSelError
    , recUpdError
    , someAsyncException
    , someException
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
import Data.Dynamic (Dynamic)
import Data.Proxy (Proxy(Proxy))
import System.Exit (ExitCode)


arithException :: Proxy ArithException
arithException = Proxy

arrayException :: Proxy ArrayException
arrayException = Proxy

assertionFailed :: Proxy AssertionFailed
assertionFailed = Proxy

asyncException :: Proxy AsyncException
asyncException = Proxy

blockedIndefinitelyOnMVar :: Proxy BlockedIndefinitelyOnMVar
blockedIndefinitelyOnMVar = Proxy

blockedIndefinitelyOnSTM :: Proxy BlockedIndefinitelyOnSTM
blockedIndefinitelyOnSTM = Proxy

deadlock :: Proxy Deadlock
deadlock = Proxy

dynamic :: Proxy Dynamic
dynamic = Proxy

errorCall :: Proxy ErrorCall
errorCall = Proxy

exitCode :: Proxy ExitCode
exitCode = Proxy

iOException :: Proxy IOException
iOException = Proxy

nestedAtomically :: Proxy NestedAtomically
nestedAtomically = Proxy

noMethodError :: Proxy NoMethodError
noMethodError = Proxy

nonTermination :: Proxy NonTermination
nonTermination = Proxy

patternMatchFail :: Proxy PatternMatchFail
patternMatchFail = Proxy

recConError :: Proxy RecConError
recConError = Proxy

recSelError :: Proxy RecSelError
recSelError = Proxy

recUpdError :: Proxy RecUpdError
recUpdError = Proxy

someAsyncException :: Proxy SomeAsyncException
someAsyncException = Proxy

someException :: Proxy SomeException
someException = Proxy
