{- |
Module      :  MIPS.ISA.Instruction
Copyright   :  (c) Jared Beller 2021
License     :  GPL-3.0-or-later
Stability   :  experimental
Portability :  portable
-}
module MIPS.ISA.Instruction (
    -- * Base instruction type
      Instruction(..)

    -- * Binary decoding
    , decodeInstruction

    -- * Instruction parameter types
    , ImmediateOperand
    , ShiftAmount
) where

import Control.Applicative ((<|>))
import Data.Word (Word16, Word32, Word8)
import MIPS.ISA.Field
import MIPS.ISA.Opcode (Decoder)
import MIPS.ISA.Opcode.Core
import MIPS.ISA.Register (Register(..), decodeRegister)

data Instruction
    = RegisterInstruction
        { instructionOpcodeR :: RegisterOpcode
        , instructionRS :: Register
        , instructionRT :: Register
        , instructionRD :: Register
        , instructionShiftAmount :: Word32 --ShiftAmount
        }
    | ImmediateInstruction
        { instructionOpcodeI :: ImmediateOpcode
        , instructionRS :: Register
        , instructionRT :: Register
        , instructionImmediate :: Word32 --ImmediateOperand
        }
    deriving (Show)

decodeInstruction :: Decoder Instruction
decodeInstruction w =
        RegisterInstruction
            <$> decodeOpcodeR w
            <*> decodeRegister (extractFieldRS w)
            <*> decodeRegister (extractFieldRT w)
            <*> decodeRegister (extractFieldRD w)
            <*> pure (extractFieldShift w)
    <|> ImmediateInstruction
            <$> decodeOpcodeI w
            <*> decodeRegister (extractFieldRS w)
            <*> decodeRegister (extractFieldRT w)
            <*> pure (extractFieldImmediate w)
    <|> fail ("Unknown opcode (" ++ showField 6 (extractFieldOpcode w) ++ ")")

type ImmediateOperand = Word16

type ShiftAmount = Word8
