{-# LANGUAGE DefaultSignatures #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Hashable.Extras
  ( Hashable1(..)
  , Hashable2(..)
  , Hashed(..)
  , hashWith1
  , hashWith2
  , salt
  ) where

import Data.Bifunctor
import Data.Functor.Identity
import Data.Hashable

data Hashed = Hashed { unhashed :: Int -> Int }

instance Hashable Hashed where
  hashWithSalt n f = unhashed f n
  {-# INLINE hashWithSalt #-}

data Salt = Salt

instance Hashable Salt where
  hashWithSalt = const
  {-# INLINE hashWithSalt #-}

-- | Extract the default salt used by `Data.Hashable`
salt :: Int
salt = hash Salt

class Hashable1 t where
  hashWithSalt1 :: Hashable a => Int -> t a -> Int
  default hashWithSalt1 :: (Hashable a, Hashable (t a)) => Int -> t a -> Int
  hashWithSalt1 = hashWithSalt1
  {-# INLINE hashWithSalt1 #-}

  hash1 :: Hashable a => t a -> Int
  hash1 = hashWithSalt1 salt
  {-# INLINE hash1 #-}

instance Hashable1 Identity where
  hashWithSalt1 n (Identity a) = hashWithSalt n a
  {-# INLINE hashWithSalt1 #-}

instance Hashable1 Maybe
instance Hashable1 []
instance Hashable a => Hashable1 (Either a)
instance Hashable a => Hashable1 ((,) a)
instance (Hashable a, Hashable b) => Hashable1 ((,,) a b)
instance (Hashable a, Hashable b, Hashable c) => Hashable1 ((,,,) a b c)
instance (Hashable a, Hashable b, Hashable c, Hashable d) => Hashable1 ((,,,,) a b c d)
instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e) => Hashable1 ((,,,,,) a b c d e)
instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f) => Hashable1 ((,,,,,,) a b c d e f)

class Hashable2 t where
  hashWithSalt2 :: (Hashable a, Hashable b) => Int -> t a b -> Int
  default hashWithSalt2 :: (Hashable a, Hashable b, Hashable (t a b)) => Int -> t a b -> Int
  hashWithSalt2 = hashWithSalt
  {-# INLINE hashWithSalt2 #-}

  hash2 :: (Hashable a, Hashable b) => t a b -> Int
  hash2 = hashWithSalt2 salt
  {-# INLINE hash2 #-}

instance Hashable2 Either
instance Hashable2 (,)
instance Hashable a => Hashable2 ((,,) a)
instance (Hashable a, Hashable b) => Hashable2 ((,,,) a b)
instance (Hashable a, Hashable b, Hashable c) => Hashable2 ((,,,,) a b c)
instance (Hashable a, Hashable b, Hashable c, Hashable d) => Hashable2 ((,,,,,) a b c d)
instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e) => Hashable2 ((,,,,,,) a b c d e)

hashWith1 :: (Functor t, Hashable1 t) => (Int -> a -> Int) -> Int -> t a -> Int
hashWith1 h s ta = s `hashWithSalt1` fmap (\a -> Hashed (`h` a)) ta
{-# INLINE hashWith1 #-}

hashWith2 :: (Bifunctor t, Hashable2 t) => (Int -> a -> Int) -> (Int -> b -> Int) -> Int -> t a b -> Int
hashWith2 h1 h2 s tab = s `hashWithSalt2` bimap (\a -> Hashed (`h1` a)) (\b -> Hashed (`h2` b)) tab
{-# INLINE hashWith2 #-}
