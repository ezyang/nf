{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.NF
-- Copyright   :  (c) Stanford University 2015
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ezyang@cs.stanford.edu
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides a data type, 'NF', representing data which has
-- been evaluated to \"Normal Form\".  This is a useful type discipline
-- for many situations when normal form data is necessary, e.g. when
-- transmitting data to other threads over channels.  If a library
-- that you are using has the following type:
--
-- @
-- strictWriteChan :: 'NFData' a => 'Control.Concurrent.Chan' a -> a -> 'IO' ()
-- @
--
-- you can specialize it to the following type in order to avoid the
-- cost of repeatedly 'deepseq'ing a value when it is not necessary:
--
-- @
-- strictWriteChan_ :: 'Control.Concurrent.Chan' ('NF' a) -> 'NF' a -> 'IO' ()
-- strictWriteChan_ = strictWriteChan
-- @
--
-- You should also consider providing APIs which only accept 'NF'
-- values, to prevent users from accidentally 'deepseq'ing.
module Data.NF (
    NF,
    makeNF, getNF,
  ) where

import Control.DeepSeq
import Data.NF.Internal

-- | Creates a value of type 'NF'.  The first time the result is
-- evaluated to whnf, the value will be 'rnf'ed.  To avoid this
-- 'rnf', see 'UnsafeNF'.
makeNF :: NFData a => a -> NF a
makeNF x = x `deepseq` UnsafeNF x

-- | Retrieves @a@ from a value of type @'NF' a@; this value
-- is guaranteed to be in normal form.
getNF :: NF a -> a
getNF (UnsafeNF a) = a
