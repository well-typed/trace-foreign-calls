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

#if !MIN_VERSION_ghc(9,9,0)

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
#if MIN_VERSION_ghc(9,5,0) && !MIN_VERSION_ghc(9,7,0)
originGenerated = Generated
#endif
#if MIN_VERSION_ghc(9,7,0) && !MIN_VERSION_ghc(9,9,0)
originGenerated = Generated SkipPmc
#endif
#if MIN_VERSION_ghc(9,9,0)
originGenerated = Generated OtherExpansion SkipPmc
#endif

{-------------------------------------------------------------------------------
  Defining modules for various symbols
-------------------------------------------------------------------------------}

modlTraceEventIO :: Module
modlTraceEventIO =
#if !MIN_VERSION_ghc(9,9,0)
    mkModule baseUnit $ mkModuleName "Debug.Trace"
#else
    mkModule ghcInternalUnit $ mkModuleName "GHC.Internal.Debug.Trace"
#endif

modlEvaluate :: Module
modlEvaluate =
#if !MIN_VERSION_ghc(9,9,0)
    mkModule baseUnit $ mkModuleName "GHC.IO"
#else
    mkModule ghcInternalUnit $ mkModuleName "GHC.Internal.IO"
#endif

modlUnsafePerformIO :: Module
modlUnsafePerformIO =
#if !MIN_VERSION_ghc(9,9,0)
    mkModule baseUnit $ mkModuleName "GHC.IO.Unsafe"
#else
    mkModule ghcInternalUnit $ mkModuleName "GHC.Internal.IO.Unsafe"
#endif

modlHasCallStack :: Module
modlHasCallStack =
#if !MIN_VERSION_ghc(9,9,0)
    mkModule baseUnit $ mkModuleName  "GHC.Stack.Types"
#else
    mkModule ghcInternalUnit $ mkModuleName  "GHC.Internal.Stack.Types"
#endif

modlCallStack :: Module
modlCallStack =
#if !MIN_VERSION_ghc(9,9,0)
    mkModule baseUnit $ mkModuleName "GHC.Stack"
#else
    mkModule ghcInternalUnit $ mkModuleName "GHC.Internal.Stack"
#endif

modlPrettyCallStack :: Module
modlPrettyCallStack =
#if !MIN_VERSION_ghc(9,9,0)
    mkModule baseUnit $ mkModuleName "GHC.Exception"
#else
    mkModule ghcInternalUnit $ mkModuleName "GHC.Internal.Stack"
#endif

