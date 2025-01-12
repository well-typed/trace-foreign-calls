{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Plugin.TraceForeignCalls.GHC.Shim (
    mkLambda
  , mkSimpleFunRhs
  , mkValBinds
  , extendValBinds
  , mkMatch
  , mkSimpleFunBind
  , checkHaveSeq
  ) where

import Data.Bifunctor (second)
import Data.Maybe (isJust)

import GHC
import GHC.Builtin.Names qualified as Names
import GHC.Data.Bag (listToBag)
import GHC.Driver.Env (hsc_HUG)
import GHC.Tc.Utils.Monad (TcGblEnv)
import GHC.Tc.Utils.Monad qualified as TC
import GHC.Types.Basic
import GHC.Types.Name.Set (mkNameSet)
import GHC.Unit.Env (lookupHugByModule)
import GHC.Unit.Types (ghcInternalUnit)

import Plugin.TraceForeignCalls.GHC.Util

mkLambda :: [LPat GhcRn] -> LHsExpr GhcRn -> LHsExpr GhcRn
#if MIN_VERSION_ghc(9,12,1)
mkLambda = mkHsLam . noLocValue
#else
mkLambda = mkHsLam
#endif

mkSimpleFunRhs :: LIdP GhcRn -> HsMatchContext (LIdP GhcRn)
mkSimpleFunRhs name = FunRhs {
      mc_fun        = name
    , mc_fixity     = Prefix
    , mc_strictness = NoSrcStrict
#if MIN_VERSION_ghc(9,12,1)
    , mc_an         = AnnFunRhs NoEpTok [] []
#endif
    }

mkValBinds :: [(RecFlag, [LHsBind GhcRn])] -> [LSig GhcRn] -> HsValBinds GhcRn
#if MIN_VERSION_ghc(9,12,1)
mkValBinds bindingGroups sigs =
    XValBindsLR $
      NValBinds bindingGroups sigs
#else
mkValBinds bindingGroups sigs =
    XValBindsLR $
      NValBinds (map (second listToBag) bindingGroups) sigs
#endif

extendValBinds ::
     [(RecFlag, [LHsBind GhcRn])]
  -> [LSig GhcRn]
  -> HsValBinds GhcRn -> HsValBinds GhcRn
#if MIN_VERSION_ghc(9,12,1)
extendValBinds newBindingGroups newSigs old =
    case old of
      XValBindsLR (NValBinds oldGroups oldSigs) ->
        XValBindsLR $
          NValBinds
            (oldGroups ++ newBindingGroups)
            (oldSigs   ++ newSigs)
      ValBinds{} ->
        error "impossible (ValBinds is only used before renaming)"
#else
extendValBinds newBindingGroups newSigs old =
    case old of
      XValBindsLR (NValBinds oldGroups oldSigs) ->
        XValBindsLR $
          NValBinds
            (oldGroups ++ map (second listToBag) newBindingGroups)
            (oldSigs   ++ newSigs)
      ValBinds{} ->
        error "impossible (ValBinds is only used before renaming)"
#endif

mkSimpleFunBind ::
     LIdP GhcRn     -- ^ Name of the function
  -> [Name]         -- ^ Free variables in the body (excluding imports)
  -> [Name]         -- ^ Arguments
  -> LHsExpr GhcRn  -- ^ Body
  -> LHsBind GhcRn
mkSimpleFunBind name freeVars args body = noLocValue $ FunBind {
      fun_ext     = mkNameSet freeVars
    , fun_id      = name
    , fun_matches = MG {
          mg_ext  = Generated OtherExpansion SkipPmc
        , mg_alts = noLocValue . map noLocValue $ [
             Match {
#if MIN_VERSION_ghc(9,12,1)
                 m_ext   = noValue
#else
                 m_ext   = noAnn
#endif
               , m_ctxt  = mkSimpleFunRhs name
#if MIN_VERSION_ghc(9,12,1)
               , m_pats  = noLocValue $ map namedVarPat args
#else
               , m_pats  = map namedVarPat args
#endif
               , m_grhss = GRHSs {
                     grhssExt        = emptyComments
                   , grhssGRHSs      = map noLocValue [
                         GRHS
                           noValue
                           [] -- guards
                           body
                       ]
                   , grhssLocalBinds = emptyWhereClause
                   }
               }
           ]
        }
    }

-- | Check if @seq#@ is available
--
-- For now we only support the use of @seq#@ (and hence profiling pure
-- functions) for GHC 9.12.
checkHaveSeq :: TcGblEnv -> HscEnv -> Bool
#if MIN_VERSION_ghc_internal(9,1201,0)
checkHaveSeq tcGblEnv hsc =
    if moduleUnit (TC.tcg_mod tcGblEnv) == ghcInternalUnit
      then isJust $ lookupHugByModule Names.gHC_INTERNAL_IO (hsc_HUG hsc)
      else True
#else
checkHaveSeq _ _ = False
#endif

