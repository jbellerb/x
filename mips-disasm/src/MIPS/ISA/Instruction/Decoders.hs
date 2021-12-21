{- |
Module      :  MIPS.ISA.Instruction.Decoders
Copyright   :  (c) Jared Beller 2021
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  portable
-}

module MIPS.ISA.Instruction.Decoders
    ( -- * General decoders for different styles of instructions
      binaryOperation
    , shiftOperation
    , registerBranchOperation
    , immediateOperation
    , constantOperation

      -- * Decoder result type
    , Decoder
    ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Word (Word16, Word32)
import MIPS.ISA.Field
import MIPS.ISA.Register (Register, decodeRegister)
import Validation (Validation, failureIf)

type Decoder a = Word32 -> Validation (NonEmpty String) a

shiftOperation :: (Register -> Register -> Word32 -> a) -> Decoder a
shiftOperation func w =
    func
        <$> decodeRegister (extractFieldRD w)
        <*> decodeRegister (extractFieldRT w)
        <*> pure (extractFieldShift w)
        <* failureIf (extractFieldRS w /= 0x00) "RS field must be zero"

registerBranchOperation :: (Register -> a) -> Decoder a
registerBranchOperation func w =
    func
        <$> decodeRegister (extractFieldRS w)
        <* failureIf (extractFieldRT w /= 0x00) "RT field must be zero"
        <* failureIf (extractFieldRD w /= 0x00) "RD field must be zero"
        <* failureIf (extractFieldShift w /= 0x00) "Jump hint must be zero"

binaryOperation :: (Register -> Register -> Register -> a) -> Decoder a
binaryOperation func w =
    func
        <$> decodeRegister (extractFieldRD w)
        <*> decodeRegister (extractFieldRS w)
        <*> decodeRegister (extractFieldRT w)
        <* failureIf (extractFieldShift w /= 0x00) "Shift field must be zero"

immediateOperation :: (Integral a) => (Register -> Register -> a -> b) -> Decoder b
immediateOperation func w =
    func
        <$> decodeRegister (extractFieldRT w)
        <*> decodeRegister (extractFieldRS w)
        <*> pure (fromIntegral (extractFieldImmediate w))

constantOperation :: (Register -> Word16 -> a) -> Decoder a
constantOperation func w =
    func
        <$> decodeRegister (extractFieldRT w)
        <*> pure (fromIntegral (extractFieldImmediate w))
        <* failureIf (extractFieldRS w /= 0x00) "RS field must be zero"
