{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ForeignFunctionInterface #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013-2014
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Hash.CRC32
  ( CRC32
  , update
  , final
  ) where

import Data.Bits
import Data.Default
import Data.Word
import Foreign.Storable
import Foreign.Ptr
import GHC.Base

------------------------------------------------------------------------------
-- CRC32
------------------------------------------------------------------------------

newtype CRC32 = CRC32 Word32

instance Default CRC32 where
  def = CRC32 0xffffffff
  {-# INLINE def #-}

update :: CRC32 -> Word8 -> CRC32
update (CRC32 h) w = CRC32 (unsafeShiftL h 8 `xor` lut w)
{-# INLINE update #-}

final :: CRC32 -> Word32
final (CRC32 h) = complement h
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
