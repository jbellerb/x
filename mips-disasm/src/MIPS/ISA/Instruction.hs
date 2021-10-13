{- |
Module      :  MIPS.ISA.Instruction
Copyright   :  (c) Jared Beller 2021
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  portable
-}

module MIPS.ISA.Instruction
    ( -- * Base instruction type
      Instruction (..)

      -- * Binary decoder
    , decodeInstruction
    ) where

import Data.Int (Int16)
import Data.Word (Word16, Word32)
import MIPS.ISA.Field (extractFieldOpcode, showField)
import MIPS.ISA.Instruction.Decoders
import MIPS.ISA.Instruction.Special (InstructionSpecial, decodeInstructionSpecial)
import MIPS.ISA.Register (Register)

data Instruction
    = AddImmediate Register Register Int16
    | AddImmediateUnsigned Register Register Int16
    | AndImmediate Register Register Word16
    | BranchEqual Register Register Int16
    | BranchNotEqual Register Register Int16
    | LoadByteUnsigned Register Register Word16
    | LoadHalfwordUnsigned Register Register Word16
    | LoadLinked Register Register Word16
    | LoadUpperImmediate Register Word16
    | LoadWord Register Register Word16
    | OrImmediate Register Register Word16
    | SetLessThanImmediate Register Register Int16
    | SetLessThanImmediateUnsigned Register Register Int16
    | Special InstructionSpecial
    | StoreByte Register Register Word16
    | StoreConditional Register Register Word16
    | StoreHalfword Register Register Word16
    | StoreWord Register Register Word16

decodeInstruction :: Word32 -> Either String Instruction
decodeInstruction w = case extractFieldOpcode w of
    0x00 -> Special <$> decodeInstructionSpecial w
    0x04 -> immediateOperation BranchEqual w
    0x05 -> immediateOperation BranchNotEqual w
    0x08 -> immediateOperation AddImmediate w
    0x09 -> immediateOperation AddImmediateUnsigned w
    0x0a -> immediateOperation SetLessThanImmediate w
    0x0b -> immediateOperation SetLessThanImmediateUnsigned w
    0x0c -> immediateOperation AndImmediate w
    0x0d -> immediateOperation OrImmediate w
    0x0f -> constantOperation LoadUpperImmediate w
    0x23 -> immediateOperation LoadWord w
    0x24 -> immediateOperation LoadByteUnsigned w
    0x25 -> immediateOperation LoadHalfwordUnsigned w
    0x28 -> immediateOperation StoreByte w
    0x29 -> immediateOperation StoreHalfword w
    0x2b -> immediateOperation StoreWord w
    0x30 -> immediateOperation LoadLinked w
    0x38 -> immediateOperation StoreConditional w
    op -> Left $ "Unknown opcode (" ++ showField 6 op ++ ")"

instance Show Instruction where
    show instruction = case instruction of
        AddImmediate rt rs imm -> showImmediate "addi" rt rs imm
        AddImmediateUnsigned rt rs imm -> showImmediate "addiu" rt rs imm
        AndImmediate rt rs imm -> showImmediate "andi" rt rs imm
        BranchEqual rt rs imm -> showImmediate "beq" rt rs imm
        BranchNotEqual rt rs imm -> showImmediate "bne" rt rs imm
        LoadByteUnsigned rt rs imm -> showOffset "lbu" rt rs imm
        LoadHalfwordUnsigned rt rs imm -> showOffset "lhu" rt rs imm
        LoadLinked rt rs imm -> showOffset "ll" rt rs imm
        LoadUpperImmediate rt imm -> showImmediateUnary "lui" rt imm
        LoadWord rt rs imm -> showOffset "lw" rt rs imm
        OrImmediate rt rs imm -> showImmediate "ori" rt rs imm
        SetLessThanImmediate rt rs imm -> showImmediate "slti" rt rs imm
        SetLessThanImmediateUnsigned rt rs imm -> showImmediate "sltiu" rt rs imm
        Special s -> show s
        StoreByte rt rs imm -> showOffset "sb" rt rs imm
        StoreConditional rt rs imm -> showOffset "sc" rt rs imm
        StoreHalfword rt rs imm -> showOffset "sh" rt rs imm
        StoreWord rt rs imm -> showOffset "sw" rt rs imm
      where
        showImmediate name a b c =
            concat [name, " ", show a, ", ", show b, ", ", show c]
        showOffset name a b o =
            concat [name, " ", show a, ", ", show o, "(", show b, ")"]
        showImmediateUnary name a b = concat [name, " ", show a, ", ", show b]
