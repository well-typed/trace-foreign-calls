# Eventlog tracing for foreign function calls

Suppose we have a module containing

```haskell
foreign import capi "test_cbits.h answer" c_answerIO :: IO CInt
```

If we compile this module with the plugin from this package by adding this to
the module header:

```
{-# OPTIONS_GHC -fplugin=Plugin.TraceForeignCalls #-}
```

then the foreign call will be wrapped, so that any call to this particular
foreign function will result in events such as this in the eventlog:

```
..
694754: cap 0: start foreign call c_answerIO
..
697444: cap 0: stop foreign call c_answerIO
..
```



