{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
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
-- This module provides the constructor for the type 'NF', allowing
-- you to create values of type 'NF' without any runtime overhead
-- if you fulfill a proof obligation that the value is already in
-- normal form.
module Data.NF.Internal(NF(..)) where

import Control.DeepSeq

-- | 'NF' is an abstract data type representing data which has been
-- evaluated to normal form.  Specifically, if a value of type @'NF' a@
-- is in weak head normal form, then it is in reduced normal form;
-- alternatively, it is only necessary to 'seq' an @'NF' a@ to assure that
-- it is fully evaluated.
newtype NF a
    -- | For @'UnsafeNF' x@ to preserve the 'NF' invariant, you must
    -- show that @'UnsafeNF' x == 'deepseq' x ('UnsafeNF' x)@.
    = UnsafeNF a

instance NFData (NF a) where
    rnf x = x `seq` ()
