{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Hash.Double
  ( Hash(..)
  , sip
  , pepper
  ) where

import Control.Applicative
import Control.Lens
import Data.Data
import Data.Hashable
import Generics.Deriving

-- $setup
-- >>> :load Data.Hash.Double
-- >>> import Control.Lens

-- | \"Less Hashing, Same Performance: Building a Better Bloom Filter\" by
-- Kirsch and Mitzenmacher demonstrated that for many use-cases, especially
-- involving Bloom filters, we can use pairwise independent hashes to
-- generate a family of related hash functions with good characteristics.
--
-- <http://www.eecs.harvard.edu/~kirsch/pubs/bbbf/rsa.pdf>
--
-- This stores a pair of hashes.
--
-- >>> sip (42 :: Int)^..taking 4 each
-- [-2574874314062730062,-9186383815474761572,2648850756822758536,-3962658744589272970]
--
-- >>> sip (42 :: Int)^.ix 3
-- -3962658744589272970
data Hash = Hash {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

sip :: Hashable a => a -> Hash
sip a = Hash (hash a)                -- hash with the salt taken from Data.Hash
             (hashWithSalt pepper a) -- chosen by fair die roll
{-# INLINE sip #-}

pepper :: Int
pepper = 0x53dffa872f4d7341

type instance Index Hash   = Int
type instance IxValue Hash = Int

instance Gettable f => Contains f Hash where
  contains i f _ = coerce $ indexed f (i :: Int) True
  {-# INLINE contains #-}

instance Gettable f => Ixed f Hash where
  ix i f (Hash a b) = coerce $ indexed f i (a + i * (b + i))
  {-# INLINE ix #-}

instance (Gettable f, Applicative f) => Each f Hash Hash Int Int where
  each f (Hash a b) = go 0 where
    go !i = indexed f i (a + i*(b+i)) *> go (i + 1)
  {-# INLINE each #-}
