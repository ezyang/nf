{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
#endif
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE RoleAnnotations #-}
#endif
#if __GLASGOW_HASKELL__ >= 710
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.NF.Internal
-- Copyright   :  (c) Stanford University 2015
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ezyang@cs.stanford.edu
-- Stability   :  experimental
-- Portability :  portable
--
-- This module is intended for internal use. Unlike "Data.NF.Unsafe",
-- this module exports the true constructor for 'NF', globally revealing
-- coerceability between @a@ and @'NF' a@, etc., in the importing module.
-- This is usually overkill, and makes it harder to avoid accidentally
-- performing invalid coercions.
module Data.NF.Internal(
#if __GLASGOW_HASKELL__ >= 800
  NF(..,NF)
#else
  NF(..)
#if __GLASGOW_HASKELL__ >= 710
  , pattern NF
#endif
#endif
) where

import Control.DeepSeq
import Data.Typeable

-- | 'NF' is an abstract data type representing data which has been
-- evaluated to normal form.  Specifically, if a value of type @'NF' a@
-- is in weak head normal form, then it is in reduced normal form;
-- alternatively, it is only necessary to 'seq' an @'NF' a@ to assure that
-- it is fully evaluated.
newtype NF a
    -- | For @'UnsafeNF' x@ to preserve the 'NF' invariant, you must
    -- show that @'UnsafeNF' x == 'deepseq' x ('UnsafeNF' x)@.
    = UnsafeNF a
    deriving (Eq, Ord, Typeable)

#if __GLASGOW_HASKELL__ >= 708
-- Suppose we have
--
-- data T = ...
-- newtype U = U T
-- instance NFData T where ...
-- instance NFData U where ... something different ...
--
-- If we let GHC infer a representational role for NF's parameter,
-- then someone could coerce from NF T (which is presumed to be in
-- T-normal form) to NF U (which is expected to be in U-normal form).
type role NF nominal
#endif

-- UGH! GHC swapped the constraint order between
-- 7.10 and 8.0. If anyone's using some beta compiler in between,
-- it might throw up.
--
-- Very unfortunately, GHC (as of 8.0) does not allow the pattern
-- synonym to be *matched* without an NFData context. Hopefully
-- this will be fixed eventually.

#if __GLASGOW_HASKELL__ >= 710
-- | Pattern synonym for the 'NF' type. Applying this synonym
-- is equivalent to calling 'Data.NF.makeNF'; matching on it
-- is equivalent to calling 'Data.NF.getNF'.
#endif
#if __GLASGOW_HASKELL__ == 710
pattern NF :: () => NFData a => a -> NF a
#elif __GLASGOW_HASKELL__ >= 800
pattern NF :: NFData a => a -> NF a
#endif

#if __GLASGOW_HASKELL__ >= 710
pattern NF a <- UnsafeNF a where
  NF a = a `deepseq` UnsafeNF a
#endif

instance NFData (NF a) where
    rnf x = x `seq` ()
