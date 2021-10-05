{- |
 Module      :  MIPS.ISA.Opcode.Core
 Copyright   :  (c) Jared Beller 2021
 License     :  GPL-3.0-or-later
 Stability   :  experimental
 Portability :  portable
-}
module MIPS.ISA.Opcode.Core (
    -- * Opcode variants
      RegisterOpcode(..)
    , ImmediateOpcode(..)

    -- * Binary decoders
    , decodeOpcodeR
    , decodeOpcodeI
) where

import Control.Applicative (empty)
import MIPS.ISA.Field (extractFieldOpcode)
import MIPS.ISA.Opcode (Decoder)
import MIPS.ISA.Opcode.Class.Special (SpecialClassCode, decodeClassCodeSpecial)

data RegisterOpcode
    = Special SpecialClassCode
    deriving (Show)

data ImmediateOpcode
    = AddImmediate
    | AddUnsignedImmediate
    | AndImmediate
    | BranchEqual
    | BranchNotEqual
    | LoadByteUnsigned
    | LoadHalfwordUnsigned
    | LoadLinked
    | LoadUpperImmediate
    | LoadWord
    | OrImmediate
    | SetLessThanImmediate
    | SetLessThanUnsignedImmediate
    | StoreByte
    | StoreConditional
    | StoreHalfword
    | StoreWord
    deriving (Show)

decodeOpcodeR :: Decoder RegisterOpcode
decodeOpcodeR w = case extractFieldOpcode w of
    0x00 -> Special <$> decodeClassCodeSpecial w
    _ -> empty

decodeOpcodeI :: Decoder ImmediateOpcode
decodeOpcodeI w = case extractFieldOpcode w of
    0x04 -> pure BranchEqual
    0x05 -> pure BranchNotEqual
    0x08 -> pure AddImmediate
    0x09 -> pure AddUnsignedImmediate
    0x0a -> pure SetLessThanImmediate
    0x0b -> pure SetLessThanUnsignedImmediate
    0x0c -> pure AndImmediate
    0x0d -> pure OrImmediate
    0x0f -> pure LoadUpperImmediate
    0x20 -> pure LoadLinked
    0x23 -> pure LoadWord
    0x24 -> pure LoadByteUnsigned
    0x25 -> pure LoadHalfwordUnsigned
    0x28 -> pure StoreByte
    0x29 -> pure StoreHalfword
    0x2b -> pure StoreWord
    0x38 -> pure StoreConditional
    _ -> empty
