{- |
 Module      :  MIPS.ISA.Opcode.Class.Special
 Copyright   :  (c) Jared Beller 2021
 License     :  GPL-3.0-or-later
 Stability   :  experimental
 Portability :  portable
-}
module MIPS.ISA.Opcode.Class.Special (
      SpecialClassCode(..)
    , decodeClassCodeSpecial
) where

import MIPS.ISA.Field (extractFieldFunc, showField)
import MIPS.ISA.Opcode (Decoder)

data SpecialClassCode
    = Add
    | AddUnsigned
    | And
    | JumpRegister
    | Nor
    | Or
    | SetLessThan
    | SetLessThanUnsigned
    | ShiftLeftLogical
    | ShiftRightLogical
    | Subtract
    | SubtractUnsigned
    deriving (Show)

decodeClassCodeSpecial :: Decoder SpecialClassCode
decodeClassCodeSpecial w = case extractFieldFunc w of
    0x00 -> pure ShiftLeftLogical
    0x02 -> pure ShiftRightLogical
    0x08 -> pure JumpRegister
    0x20 -> pure Add
    0x21 -> pure AddUnsigned
    0x22 -> pure Subtract
    0x23 -> pure SubtractUnsigned
    0x24 -> pure And
    0x25 -> pure Or
    0x27 -> pure Nor
    0x2a -> pure SetLessThan
    0x2b -> pure SetLessThanUnsigned
    e -> fail $ "Invalid SPECIAL function (" ++ showField 6 e ++ ")"
