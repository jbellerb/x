{- |
Module      :  MIPS.ISA.Instruction.Special
Copyright   :  (c) Jared Beller 2021
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  portable
-}

module MIPS.ISA.Instruction.Special
    ( -- * Class-specific instruction type
      InstructionSpecial (..)

      -- * Binary decoder
    , decodeInstructionSpecial
    ) where

import Data.Word (Word32)
import MIPS.ISA.Field (extractFieldFunction, showField)
import MIPS.ISA.Instruction.Decoders
import MIPS.ISA.Register (Register)

data InstructionSpecial
    = Add Register Register Register
    | AddUnsigned Register Register Register
    | And Register Register Register
    | JumpRegister Register
    | NotOr Register Register Register
    | Or Register Register Register
    | SetLessThan Register Register Register
    | SetLessThanUnsigned Register Register Register
    | ShiftLeftLogical Register Register Word32
    | ShiftRightLogical Register Register Word32
    | Subtract Register Register Register
    | SubtractUnsigned Register Register Register

decodeInstructionSpecial :: Word32 -> Either String InstructionSpecial
decodeInstructionSpecial w = case extractFieldFunction w of
    0x00 -> shiftOperation ShiftLeftLogical w
    0x02 -> shiftOperation ShiftRightLogical w
    0x08 -> registerBranchOperation JumpRegister w
    0x20 -> binaryOperation Add w
    0x21 -> binaryOperation AddUnsigned w
    0x22 -> binaryOperation Subtract w
    0x23 -> binaryOperation SubtractUnsigned w
    0x24 -> binaryOperation And w
    0x25 -> binaryOperation Or w
    0x27 -> binaryOperation NotOr w
    0x2a -> binaryOperation SetLessThan w
    0x2b -> binaryOperation SetLessThanUnsigned w
    func -> Left $ "Invalid SPECIAL function (" ++ showField 6 func ++ ")"

instance Show InstructionSpecial where
    show instruction = case instruction of
        Add rd rs rt -> showBinary "add" rd rs rt
        AddUnsigned rd rs rt -> showBinary "addi" rd rs rt
        And rd rs rt -> showBinary "and" rd rs rt
        JumpRegister rs -> showUnary "jr" rs
        NotOr rd rs rt -> showBinary "nor" rd rs rt
        Or rd rs rt -> showBinary "or" rd rs rt
        SetLessThan rd rs rt -> showBinary "slt" rd rs rt
        SetLessThanUnsigned rd rs rt -> showBinary "sltu" rd rs rt
        ShiftLeftLogical rd rt sa -> showBinary "sll" rd rt sa
        ShiftRightLogical rd rt sa -> showBinary "srl" rd rt sa
        Subtract rd rs rt -> showBinary "sub" rd rs rt
        SubtractUnsigned rd rs rt -> showBinary "subu" rd rs rt
      where
        showUnary name a = concat [name, " ", show a]
        showBinary name a b c =
            concat [name, " ", show a, ", ", show b, ", ", show c]
