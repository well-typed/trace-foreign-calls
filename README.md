# Trace foreign calls

## Overview

Suppose we have a `foreign import` such as

```haskell
foreign import capi "cbits.h xkcdRandomNumber" someForeignFunInA :: IO CInt
```

If the module containing the import is compiled with this plugin enabled, this
foreign function will be wrapped in a function that emits custom events to the
eventlog before and after the foreign call is made. If you run your executable
with

```bash
$ cabal run your-executable -- +RTS -l
```

and then inspect the eventlog with
[`ghc-events`](https://hackage.haskell.org/package/ghc-events) `show`, you will
see something like this:

```
..
397876: cap 0: running thread 1
491265: cap 0: trace-foreign-calls: call someForeignFunInA (capi safe "cbits.h xkcdRandomNumber") at CallStack (from HasCallStack):
  someForeignFunInA, called at src/ExamplePkgB.hs:11:21 in example-pkg-B-0.1.0-inplace:ExamplePkgB
491815: cap 0: stopping thread 1 (making a foreign call)
492165: cap 0: running thread 1
500755: cap 0: trace-foreign-calls: return someForeignFunInA
..
```

Of course any other tooling for the eventlog, such as
[`threadscope`](https://hackage.haskell.org/package/threadscope), will be able
to see these events as well.

## Enabling the plugin for your package

Add a dependency to the `build-depends` of your `.cabal` file

```cabal
  build-depends:
      ..
      trace-foreign-calls
      ..
```

and then enable the module either globally by adding

```cabal
  ghc-options:
      -fplugin=Plugin.TraceForeignCalls
```

to your `.cabal` file, or on a per-module basis by adding this pragma to the
module header:

```haskell
{-# OPTIONS_GHC -fplugin=Plugin.TraceForeignCalls #-}
```

## Plugin options

If you want to see how the plugin transforms your code, you can add a plugin
option

```haskell
{-# OPTIONS_GHC -fplugin=Plugin.TraceForeignCalls
                -fplugin-opt Plugin.TraceForeignCalls:dump-generated #-}
```

You can disable `HasCallStack` support by setting

```
{-# OPTIONS_GHC -fplugin-opt Plugin.TraceForeignCalls:disable-callstack #-}
```

## Enabling the plugin on all (transitive) dependencies

In an ideal world, we could just create a `cabal.project` file containing

```cabal
package *
  ghc-options:
    -fplugin-trustworthy
    -plugin-package=trace-foreign-calls
    -fplugin=Plugin.TraceForeignCalls
```

The first open ensures that if we have dependencies that rely on Safe Haskell,
compiling modules with the plugin does not mark them as unsafe, the second line
declares which package the plugin comes from, and finally the third line
enables the plugin.

Unfortunately, this is not quite sufficient. The problem is that we have not
edited the `.cabal` files of all packages and declared `trace-foreign-calls` to
be a dependency. We _could_ do that, but of course that would be extremely
laborious. There are some `cabal` tickets open about solving this properly
([#6881](https://github.com/haskell/cabal/issues/6881),
[#7901](https://github.com/haskell/cabal/issues/7901)), but for now we need to
use a workaround.

First, we will install the `plugin` in a fresh `cabal` store:

```bash
$ cabal --store-dir=/tmp/cabal-plugin-store install --lib trace-foreign-calls
```

Create a `cabal.project.plugin` file with

```cabal
import: cabal.project

package *
  ghc-options:
    -package-db=/tmp/cabal-plugin-store/ghc-9.6.4/package.db
    -fplugin-trustworthy
    -plugin-package=trace-foreign-calls
    -fplugin=Plugin.TraceForeignCalls

store-dir: /tmp/cabal-plugin-store
```

You should then be able to build or run your executable, rebuilding (almost)
all of its dependencies, with

```bash
$ cabal run --project-file cabal.project.plugin
```

## Upgrading the plugin

When you install a new version of the plugin, `cabal` will not try to rebuild
any dependencies (it does not include the hash of the plugin in the hash of the
packages). So wipe your `cabal-plugin-store` as well as your `dist-newstyle`
directory each time you update your plugin (another good reason for using a
separate store for the plugin).

## `libphread`

For reasons currently unclear, enabling the plugin on packages that declare

```cabal
extra-libraries: pthread
```

in their `.cabal` file will cause a compilation failure:

```
<command line>: User-specified static library could not be loaded (/usr/lib/gcc/x86_64-linux-gnu/13/../../../x86_64-linux-gnu/libpthread.a)
Loading static libraries is not supported in this configuration.
Try using a dynamic library instead.
```

Currently the only known workaround is patch such packages and replace this with

```cabal
cc-options: -pthread
```

An example is `crypton`, see
[crypton#33](https://github.com/kazu-yamamoto/crypton/pull/33) for details.

