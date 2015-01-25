Proxies
=======

[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)][Haskell.org]
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)][tl;dr Legal: BSD3]

[![Build](https://travis-ci.org/trskop/type-proxies.svg)](https://travis-ci.org/trskop/type-proxies)


Description
-----------

Concrete type proxies for various data types. Type proxies allows us to use
types as a values, e.g. passing it as an argument of a function.

Small example where this can be usefull:

````Haskell
-- | Run 'someAction' and wrap its result in 'Just', if it raises an exception,
-- then return 'Nothing'.
doSomeAction :: IO (Maybe Result)
doSomeAction = (Just <$> someAction)
    `catch` forget ioException (return Nothing)
````

Notice that `forget` function is a type restricted `const`; its type signature
is:

````Haskell
forget :: Proxy b -> a -> b -> a
````

As you can see it takes type proxy argument and that restricts the type of
ignored argument.



[Haskell.org]:
  http://www.haskell.org
  "The Haskell Programming Language"
[tl;dr Legal: BSD3]:
  https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29
  "BSD 3-Clause License (Revised)"
