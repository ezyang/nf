{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.DontForce
-- Copyright   :  (c) David Feuer 2016
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ezyang@cs.stanford.edu
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides a type for values that should always
-- be considered to be in normal form, without ever forcing
-- them. This is a bit of a dirty trick for forcing some outer
-- structure without forcing whatever is inside.
--
-- forceOutside :: Functor f => f a -> ()
-- forceOutside xs = rnf (fmap DontForce xs)
--
-- Note that this relies on the NFData instance for the functor
-- using only 'rnf', and never 'seq', on its elements. This should
-- generally be the case.
module Data.DontForce where
import Data.Typeable (Typeable)
import Control.DeepSeq (NFData (..))
#if __GLASGOW_HASKELL__ >= 702
import GHC.Generics (Generic)
#endif
#if __GLASGOW_HASKELL__ >= 706
import GHC.Generics (Generic1)
#endif

newtype DontForce a = DontForce a deriving (Eq, Ord, Typeable)

instance NFData (DontForce a) where
  rnf _ = ()

#if __GLASGOW_HASKELL__ >= 702
deriving instance Generic (DontForce a)
#endif
#if __GLASGOW_HASKELL__ >= 706
deriving instance Generic1 DontForce
#endif
