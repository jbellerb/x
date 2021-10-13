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
    ) where

import Data.Word (Word16, Word32)
import MIPS.ISA.Field
import MIPS.ISA.Register (Register, decodeRegister)

expect :: Bool -> String -> Either String ()
expect True _ = pure ()
expect False s = Left s

shiftOperation :: (Register -> Register -> Word32 -> a) -> Word32 -> Either String a
shiftOperation func w = do
    expect (extractFieldRS w == 0x00) "RS field must be zero"
    rt <- decodeRegister $ extractFieldRT w
    rd <- decodeRegister $ extractFieldRD w
    let sa = extractFieldShift w
    pure $ func rd rt sa

registerBranchOperation :: (Register -> a) -> Word32 -> Either String a
registerBranchOperation func w = do
    rs <- decodeRegister $ extractFieldRS w
    expect (extractFieldRT w == 0x00) "RT field must be zero"
    expect (extractFieldRD w == 0x00) "RD field must be zero"
    expect (extractFieldShift w == 0x00) "Jump hint must be zero"
    pure $ func rs

binaryOperation :: (Register -> Register -> Register -> a) -> Word32 -> Either String a
binaryOperation func w = do
    rs <- decodeRegister $ extractFieldRS w
    rt <- decodeRegister $ extractFieldRT w
    rd <- decodeRegister $ extractFieldRD w
    expect (extractFieldShift w == 0x00) "Shift field must be zero"
    pure $ func rd rs rt

immediateOperation :: (Integral a) => (Register -> Register -> a -> b) -> Word32 -> Either String b
immediateOperation func w = do
    rs <- decodeRegister $ extractFieldRS w
    rt <- decodeRegister $ extractFieldRT w
    let imm = fromIntegral $ extractFieldImmediate w
    pure $ func rt rs imm

constantOperation :: (Register -> Word16 -> a) -> Word32 -> Either String a
constantOperation func w = do
    expect (extractFieldRS w == 0x00) "RS field must be zero"
    rt <- decodeRegister $ extractFieldRT w
    let imm = fromIntegral $ extractFieldImmediate w
    pure $ func rt imm
