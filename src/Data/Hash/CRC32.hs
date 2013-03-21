{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ForeignFunctionInterface #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Hash.CRC32
  ( CRC32
  , updated
  , final
  ) where

import Control.Lens
import Data.Bits
import Data.Default
import Data.Monoid
import Data.Word
import Foreign.Storable
import Foreign.Ptr
import GHC.Base

------------------------------------------------------------------------------
-- CRC32
------------------------------------------------------------------------------

newtype CRC32 = CRC32 { getCRC32 :: Word32 }

instance Default CRC32 where
  def = CRC32 0xffffffff;
  {-# INLINE def #-}

instance (Bifunctor p, Profunctor p, Functor f) => Snoc p f CRC32 CRC32 Word8 Word8 where
  _Snoc = unto $ \(CRC32 h, w) -> CRC32 (shiftL h 8 `xor` lut w)
  {-# INLINE _Snoc #-}

updated :: Getting (Endo (Endo CRC32)) t Word8 -> t -> CRC32 -> CRC32
updated l t z = foldlOf' l snoc z t
{-# INLINE updated #-}

final :: CRC32 -> Word32
final = complement . getCRC32
{-# INLINE final #-}

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

foreign import ccall "static &crc32_lut" crc32_lut :: Ptr Word32

#ifndef HLINT
inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of
  (# _, r #) -> r
{-# INLINE inlinePerformIO #-}
#endif

lut :: Word8 -> Word32
lut i = inlinePerformIO (peekElemOff crc32_lut (fromIntegral i))
{-# INLINE lut #-}
