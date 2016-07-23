{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE PatternSynonyms #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.NF.Unsafe
-- Copyright   :  (c) David Feuer 2016
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ezyang@cs.stanford.edu
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides an unsafe pattern synonym for the type 'NF', allowing you
-- to create values of type 'NF' without any runtime overhead if you fulfill a
-- proof obligation that the value is already in normal form. It also provides
-- functions allowing more general coercions relating to 'NF'.
--
-- This module should generally be preferred to "Data.NF.Internal" because
-- the bindings defined here allow safety to be defeated locally, on
-- an as-needed basis, without making the 'Coercible' instances available
-- throughout a module.
module Data.NF.Unsafe (
    unsafeMakeNF
#if __GLASGOW_HASKELL__ >= 708
  , coercionNF
  , coercionUnderNF
  , pattern UnsafeNF
#endif
  ) where

import qualified Data.NF.Internal as Internal
import Data.NF.Internal (NF)
#if __GLASGOW_HASKELL__ >= 708
import Data.Coerce (Coercible)
import Data.Type.Coercion (Coercion (..))
#endif

-- | Convert a value of type @a@ to one of type @NF a@
-- without forcing it first.
unsafeMakeNF :: a -> NF a
unsafeMakeNF = Internal.UnsafeNF

#if __GLASGOW_HASKELL__ >= 708
-- | Produce a coercion from @a@ to @NF a@.
--
-- Since the constructor for 'NF' is not exported by this module,
-- there is no @Coercible a (NF a)@ instance "in the air". This
-- binding exposes one.
coercionNF :: Coercion a (NF a)
coercionNF = Coercion

-- | Produce a coercion from one 'NF' type to another.
--
-- Since 'NF' takes a nominal type parameter, it is
-- not generally possible to 'coerce' from @NF a@ to
-- @NF b@, even when @Coercible a b@.
--
-- prop> coercionUnderNF = trans (sym wrappingCoercion) (trans Coercion wrappingCoercion)
--
-- Example:
--
-- @
-- data X = ...
-- newtype Y = Y X
-- instance NFData X where ... 
-- instance NFData Y where
--   rnf (Y x) = rnf x
-- @
--
-- Since @X@ and @Y@ use equivalent implementations of 'rnf',
-- it is safe to coerce between @NF X@ and @NF Y@. One can
-- reveal this locally as follows:
--
-- @
-- case coercionUnderNF :: Coercion (NF X) (NF Y) of
--   Coercion -> ...
-- @
coercionUnderNF :: Coercible a b => Coercion (NF a) (NF b)
coercionUnderNF = Coercion

-- | Pattern synonym for 'NF' allowing the type to be constructed
-- and deconstructed without forcing.
#if __GLASGOW_HASKELL__ >= 710
pattern UnsafeNF :: a -> NF a
#endif
pattern UnsafeNF a = Internal.UnsafeNF a
#endif
