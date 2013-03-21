{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ForeignFunctionInterface #-}

#ifndef MIN_VERSION_lens
#define MIN_VERSION_lens(x,y,z) 1
#endif
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

instance (Reviewable p, Functor f) => Snoc p f CRC32 CRC32 Word8 Word8 where
  _Snoc = unto $ \(CRC32 h, w) -> CRC32 (shiftL h 8 `xor` lut w)
  {-# INLINE _Snoc #-}

#if MIN_VERSION_lens(3,9,0)
updated :: Getting (Endo (Endo CRC32)) t Word8 -> t -> CRC32 -> CRC32
#else
updated :: Getting (Endo (Endo CRC32)) t t Word8 Word8 -> t -> CRC32 -> CRC32
#endif
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
