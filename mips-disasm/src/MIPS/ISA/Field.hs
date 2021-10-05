{- |
 Module      :  MIPS.ISA.Field
 Copyright   :  (c) Jared Beller 2021
 License     :  GPL-3.0-or-later
 Stability   :  experimental
 Portability :  portable
-}
module MIPS.ISA.Field (
    -- * Hex printer
      showField

    -- * Instruction field extractors
    , extractFieldFunc
    , extractFieldImmediate
    , extractFieldOpcode
    , extractFieldRD
    , extractFieldRS
    , extractFieldRT
    , extractFieldShift
) where

import Data.Bits (shift, (.&.))
import Data.Word (Word32)
import Numeric (showHex)

showField :: Int -> Word32 -> String
showField n w = "0x" ++ foldl (flip showHex) "" (nibbles n w)
  where
    nibbles n' w'
        | n' <= 0 = []
        | otherwise = w' .&. 0x0f : nibbles (n' - 4) (shift w' (-4))

extractFieldFunc :: Word32 -> Word32
extractFieldFunc w = w .&. 0x3f

extractFieldImmediate :: Word32 -> Word32
extractFieldImmediate w = w .&. 0x00ff

extractFieldOpcode :: Word32 -> Word32
extractFieldOpcode w = shift w (-26) .&. 0x3f

extractFieldRD :: Word32 -> Word32
extractFieldRD w = shift w (-11) .&. 0x1f

extractFieldRS :: Word32 -> Word32
extractFieldRS w = shift w (-21) .&. 0x1f

extractFieldRT :: Word32 -> Word32
extractFieldRT w = shift w (-16) .&. 0x1f

extractFieldShift :: Word32 -> Word32
extractFieldShift w = shift w (-6) .&. 0x1f
