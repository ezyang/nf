-- | Since 'NF' takes a nominal type parameter, it is
-- not generally possible to 'coerce' from @NF a@ to
-- @NF b@, even when @Coercible a b@. Importing 'Data.NF.Internal'
-- allows ''all'' such coercions. This module allows the coercions
-- locally, in a controlled fashion.
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
-- case nfCoercion :: Coercion (NF X) (NF Y) of
--   Coercion -> ...
-- @
module Data.NF.Coercion (nfCoercion) where

import Data.NF.Internal (NF (..))
import Data.Coerce (Coercible)
import Data.Type.Coercion (Coercion (..))

-- | Produce a coercion from one 'NF' type to another.
nfCoercion :: Coercible a b => Coercion (NF a) (NF b)
nfCoercion = Coercion
