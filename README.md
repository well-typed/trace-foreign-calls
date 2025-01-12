# Trace foreign calls

## Overview

The `trace-foreign-calls` compiler plugin transforms your code, replacing all
foreign imports

```haskell
foreign import capi "foo" c_foo :: ..
```

by

```haskell
foreign import capi "foo" c_foo_uninstrumented :: ..
```

alongside a wrapper

```haskell
c_foo :: ..
```

which calls the original FFI function, but additionally emits eventlog events
before and after the foreign function invocation:

```
1769223930: cap 2: trace-foreign-calls: call c_foo (capi safe "foo")
...
2206695620: cap 2: trace-foreign-calls: return c_foo
```

This makes it possible to profile the time spent in foreign calls, either by
processing the event log yourself or by using
[ghc-events-util](https://github.com/well-typed/ghc-events-util).

## Limitations and future work

* Requires GHC 9.12
* Standard time profiling tools can _NOT_ be used on the eventlog.
* It is not possible to profile Haskell functions and FFI functions at the
  same time.

Some of these limitations arise from the fact that we re-using the existing
"heap profile sample" event for a different purpose, which would confuse
existing time profiling tools. A better solution would be to add support for
profiling foreign functions to GHC itself. This would involve adding new types
of eventlog events, corresponding primops to generate them, and update existing
time profiling tooling to interpret those events.

## Usage

### Enabling the plugin

Add a dependency to the `build-depends` of your `.cabal` file

```cabal
  build-depends: .., trace-foreign-calls
```

and then enable the module either globally by adding

```cabal
  ghc-options: -fplugin=Plugin.TraceForeignCalls
```

to your `.cabal` file, or on a per-module basis by adding this pragma to the
module header:

```haskell
{-# OPTIONS_GHC -fplugin=Plugin.TraceForeignCalls #-}
```

### Plugin options

If you want to see how the plugin transforms your code, you can add a plugin
option

```haskell
{-# OPTIONS_GHC -fplugin=Plugin.TraceForeignCalls
                -fplugin-opt Plugin.TraceForeignCalls:dump-generated #-}
```

### Running your code

To run your application, make sure to pass the `-l` runtime flag:

```
cabal run your-application -- +RTS -l
```

### Callstacks

The plugin will generate a (custom) `call` and `return` event each time a
foreign call is made. To additionally also get a cost-centre callstack, compile
your application with profiling enabled, but do _not_ enbale the `-p` runtime
flag when running it:

```
cabal run your-application --enable-profiling -- +RTS -l
```

```
1769223930: cap 2: trace-foreign-calls: call c_foo (capi safe "foo")
1769224110: heap prof sample 0, residency 2, cost centre stack 29765, 29855, 29768, 24306
...
2206695620: cap 2: trace-foreign-calls: return c_foo
```

The heap sample event we generate is not a true profiling event, and cannot be
processed by standard time profiling tooling. The stack is a true cost-centre
stack, but we leave the `profile` field at zero and abuse the `residency` field
to instead record the capability (this allows us to correlative concurrent
foreign calls).

### Concurrency

If your application does any kind of concurrency, make sure to compile your
application with `-threaded`, and run with

```
cabal run your-application [--enable-profiling] -- +RTS -l -N
```

This will ensure that concurrent foreign calls will run on different
capabilities, making it easier to correlate interleaved `call` and `return`
events.

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
cabal --store-dir=/tmp/cabal-plugin-store install \
  --lib trace-foreign-calls \
  --package-env .
```

Create a `cabal.project.plugin` file with

```cabal
import: cabal.project

package *
  ghc-options:
    -package-db=/tmp/cabal-plugin-store/ghc-9.12.1-a75a/package.db
    -fplugin-trustworthy
    -plugin-package=trace-foreign-calls
    -fplugin=Plugin.TraceForeignCalls

store-dir: /tmp/cabal-plugin-store
```

You should then be able to build or run your executable, rebuilding (almost)
all of its dependencies, with

```bash
$ cabal run --project-file cabal.project.plugin <your-exec>
```

### Upgrading the plugin

When you install a new version of the plugin, `cabal` will not try to rebuild
any dependencies (it does not include the hash of the plugin in the hash of the
packages). So wipe your `cabal-plugin-store` as well as your `dist-newstyle`
directory each time you update your plugin (another good reason for using a
separate store for the plugin).

## The generated wrappers

### IO function, no profiling

An IO function such as

```haskell
foreign import capi "test_cbits.h slow_add"
  c_slowAddIO :: CLong -> CLong -> IO CLong
```

gets replaced by

```haskell
foreign import capi "test_cbits.h slow_add"
  c_slowAddIO_uninstrumented :: CLong -> CLong -> IO CLong
```

and the following wrapper is generated:

```haskell
c_slowAddIO :: CLong -> CLong -> IO CLong
c_slowAddIO x y =
    case c_slowAddIO_uninstrumented x y of IO f -> IO $ \s0 ->

    case traceEvent# call   s0 of    s1            ->
    case f                  s1 of (# s2, result #) ->
    case traceEvent# return s2 of    s3            ->

    (# s3, result #)
  where
    call   = "trace-foreign-calls: call c_slowAddIO (capi safe \"test_cbits.h slow_add\")"#
    return = "trace-foreign-calls: return c_slowAddIO"#
```

### Profiling

When profiling is enabled, we generate some additional calls:

```haskell
c_slowAddIO :: CLong -> CLong -> IO CLong
c_slowAddIO x y =
    case c_slowAddIO_uninstrumented x y of IO f -> IO $ \s0 ->

    case traceEvent# call  s0 of    s1               ->
    case getCurrentCCS# f  s1 of (# s2, ccs #)       ->
    case myThreadId#       s2 of (# s3, tid #)       ->
    case threadStatus# tid s3 of (# s4, _, cap, _ #) ->

    case traceCCS# 0#Word8 ccs (int64ToWord64# (intToInt64# cap)) of IO runTrace ->

    case runTrace           s4 of (# s5, _unit #)  ->
    case f                  s5 of (# s6, result #) ->
    case traceEvent# return s6 of    s7 ->

    (# s7, result #)
```

### Pure functions

For a pure foreign import

```haskell
foreign import capi "test_cbits.h slow_add"
  c_slowAddPure :: CLong -> CLong -> CLong
```

we generate nearly the same wrapper, except that it starts with

```haskell
let f = c_slowAddPure_uninstrumented x y in ..
```

and we call the function using

```haskell
case seq# f s of (# s', result #) -> ..
```

The wrapper is otherwise identical (with or without profiling).

## Tests

### Running the test suite

To run the test suite, use

```
cabal run test-trace-foreign-calls [--enable-profiling]
```

The test suite is mostly there to verify that the code generated by the
plugin compiles; we make no effort to inspect the eventlog. To do this manually,
you can use

```
ghc-events show test-trace-foreign-calls.eventlog
```

and look for `trace-foreign-calls` events.

### Compiling transitive dependencies

Set things up as described above; then run

```
cabal run --project-file cabal.project.plugin-9.12.1 example-pkg-B -- +RTS -l
```

Then `test-B.eventlog` should contain `trace-foreign-calls` events for both
`someForeignFunInA` (defined in `example-pkg-A`) as well as various `zlib`
related functions such as `c_zlibVersion`.

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

Currently the only known workaround is patch such packages; it many cases it may
be possible to simply remove `pthread` from `extra-libraries`; alternatively, it
may be possible to instead use `cc-options`:

```cabal
cc-options: -pthread
```

An example used to be `crypton`; see
[crypton#32](https://github.com/kazu-yamamoto/crypton/pull/32) and
[crypton#33](https://github.com/kazu-yamamoto/crypton/pull/33) for examples of
both of these options, and see https://stackoverflow.com/a/62561519/742991 for a
discussion of the difference between `-lphread` and `-pthread`.
