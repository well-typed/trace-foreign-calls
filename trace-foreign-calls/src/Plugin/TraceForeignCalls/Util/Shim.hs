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
  , modlSeq
  , modlHasCallStack
  , modlPrettyCallStack
  , modlTraceEvent
  , modlRunRW
  , modlNoDuplicate
  , modlGetCurrentCCS
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

#if MIN_VERSION_ghc(9,12,0)
instance NoValue EpaLocation where
  noValue = noAnn
#else
instance NoValue [AddEpAnn] where
  noValue = []
#endif

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

modlTraceEvent :: Module
modlTraceEvent = mkModule primUnit $ mkModuleName "GHC.Prim"

modlSeq :: Module
#if MIN_VERSION_ghc(9,12,0)
modlSeq = mkModule ghcInternalUnit $ mkModuleName "GHC.Internal.IO"
#else
modlSeq = mkModule primUnit $ mkModuleName "GHC.Prim"
#endif

modlRunRW :: Module
modlRunRW = mkModule primUnit $ mkModuleName "GHC.Magic"

modlNoDuplicate :: Module
modlNoDuplicate = mkModule primUnit $ mkModuleName "GHC.Prim"

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

modlGetCurrentCCS :: Module
modlGetCurrentCCS =
    mkModule primUnit $ mkModuleName "GHC.Prim"

