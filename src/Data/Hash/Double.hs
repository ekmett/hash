{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013-2014
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
  , hashes
  , hashed
  ) where

import Data.Data
import Data.Hashable
import GHC.Generics

-- $setup
-- >>> :load Data.Hash.Double

-- | \"Less Hashing, Same Performance: Building a Better Bloom Filter\" by
-- Kirsch and Mitzenmacher demonstrated that for many use-cases, especially
-- involving Bloom filters, we can use pairwise independent hashes to
-- generate a family of related hash functions with good characteristics.
--
-- <http://www.eecs.harvard.edu/~kirsch/pubs/bbbf/rsa.pdf>
--
-- This stores a pair of hashes.
--
-- >>> take 4 $ hashes $ sip (42 :: Int)
-- [42,-8014524686259163228,2417694701191225120,-5596829985067938146]
--
-- >>> hashes (sip (42 :: Int)) !! 3
-- -5596829985067938146
data Hash = Hash {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

sip :: Hashable a => a -> Hash
sip a = Hash (hash a)                -- hash with the salt taken from Data.Hash
             (hashWithSalt pepper a) -- chosen by fair die roll
{-# INLINE sip #-}

pepper :: Int
pepper = 0x53dffa872f4d7341

hashes :: Hash -> [Int]
hashes (Hash a0 b) = go a0 0 where
  go !a !i = a + i*i : go (a + b) (i + 1)
{-# INLINE hashes #-}

hashed :: Hash -> Int -> Int
hashed (Hash a b) i = a + i*(b+i)
{-# INLINE hashed #-}
