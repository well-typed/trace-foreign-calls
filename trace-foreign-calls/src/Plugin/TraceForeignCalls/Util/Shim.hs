{-# LANGUAGE CPP #-}

-- | GHC shim
--
-- All CPP should live in this module.
module Plugin.TraceForeignCalls.Util.Shim (
    -- * Constructing the AST
    noValue
  , noLocValue
  , originGenerated
    -- * Name resolution
  , modlCallStack
  , modlEvaluate
  , modlHasCallStack
  , modlPrettyCallStack
  , modlTraceEventIO
  , modlUnsafePerformIO
  ) where

import GHC
import GHC.Plugins

{-------------------------------------------------------------------------------
  Annotations
-------------------------------------------------------------------------------}

class NoValue a where
  -- | Value that provides no additional information
  noValue :: a

noLocValue :: NoValue l => e -> GenLocated l e
noLocValue = L noValue

instance NoValue NoExtField where
  noValue = NoExtField

instance NoValue SrcSpan where
  noValue = noSrcSpan

#if __GLASGOW_HASKELL__ < 910

instance NoValue a => NoValue (SrcSpanAnn' a) where
  noValue = SrcSpanAnn noValue noValue

instance NoValue (EpAnn ann) where
  noValue = EpAnnNotUsed

#else

instance NoAnn ann => NoValue (EpAnn ann) where
  noValue = noAnn

instance NoValue AnnSig where
  noValue = noAnn

instance NoValue [AddEpAnn] where
  noValue = []

#endif

{-------------------------------------------------------------------------------
  Origin
-------------------------------------------------------------------------------}

originGenerated :: Origin
#if __GLASGOW_HASKELL__ == 906
originGenerated = Generated
#endif
#if __GLASGOW_HASKELL__ == 908
originGenerated = Generated SkipPmc
#endif
#if __GLASGOW_HASKELL__ >= 910
originGenerated = Generated OtherExpansion SkipPmc
#endif

{-------------------------------------------------------------------------------
  Defining modules for various symbols
-------------------------------------------------------------------------------}

modlTraceEventIO :: Module
modlTraceEventIO =
#if __GLASGOW_HASKELL__ < 910
    mkModule baseUnit $ mkModuleName "Debug.Trace"
#else
    mkModule ghcInternalUnit $ mkModuleName "GHC.Internal.Debug.Trace"
#endif

modlEvaluate :: Module
modlEvaluate =
#if __GLASGOW_HASKELL__ < 910
    mkModule baseUnit $ mkModuleName "GHC.IO"
#else
    mkModule ghcInternalUnit $ mkModuleName "GHC.Internal.IO"
#endif

modlUnsafePerformIO :: Module
modlUnsafePerformIO =
#if __GLASGOW_HASKELL__ < 910
    mkModule baseUnit $ mkModuleName "GHC.IO.Unsafe"
#else
    mkModule ghcInternalUnit $ mkModuleName "GHC.Internal.IO.Unsafe"
#endif

modlHasCallStack :: Module
modlHasCallStack =
#if __GLASGOW_HASKELL__ < 910
    mkModule baseUnit $ mkModuleName  "GHC.Stack.Types"
#else
    mkModule ghcInternalUnit $ mkModuleName  "GHC.Internal.Stack.Types"
#endif

modlCallStack :: Module
modlCallStack =
#if __GLASGOW_HASKELL__ < 910
    mkModule baseUnit $ mkModuleName "GHC.Stack"
#else
    mkModule ghcInternalUnit $ mkModuleName "GHC.Internal.Stack"
#endif

modlPrettyCallStack :: Module
modlPrettyCallStack =
#if __GLASGOW_HASKELL__ < 910
    mkModule baseUnit $ mkModuleName "GHC.Exception"
#else
    mkModule ghcInternalUnit $ mkModuleName "GHC.Internal.Stack"
#endif

