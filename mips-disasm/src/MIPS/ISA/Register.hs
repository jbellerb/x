{- |
Module      :  MIPS.ISA.Register
Copyright   :  (c) Jared Beller 2021
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  portable
-}

module MIPS.ISA.Register
    ( Register (..)

      -- * Binary decoding
    , decodeRegister
    ) where

import Data.Word (Word32)

data Register
    = Zero
    | AssemblerTemporary
    | ValueResult0
    | ValueResult1
    | Argument0
    | Argument1
    | Argument2
    | Argument3
    | Temporary0
    | Temporary1
    | Temporary2
    | Temporary3
    | Temporary4
    | Temporary5
    | Temporary6
    | Temporary7
    | SavedTemporary0
    | SavedTemporary1
    | SavedTemporary2
    | SavedTemporary3
    | SavedTemporary4
    | SavedTemporary5
    | SavedTemporary6
    | SavedTemporary7
    | Temporary8
    | Temporary9
    | Kernel0
    | Kernel1
    | GlobalPointer
    | StackPointer
    | FramePointer
    | ReturnAddress
    deriving (Enum)

instance Show Register where
    show register = case register of
        Zero -> "$zero"
        AssemblerTemporary -> "$at"
        ValueResult0 -> "$v0"
        ValueResult1 -> "$v1"
        Argument0 -> "$a0"
        Argument1 -> "$a1"
        Argument2 -> "$a2"
        Argument3 -> "$a3"
        Temporary0 -> "$t0"
        Temporary1 -> "$t1"
        Temporary2 -> "$t2"
        Temporary3 -> "$t3"
        Temporary4 -> "$t4"
        Temporary5 -> "$t5"
        Temporary6 -> "$t6"
        Temporary7 -> "$t7"
        SavedTemporary0 -> "$s0"
        SavedTemporary1 -> "$s1"
        SavedTemporary2 -> "$s2"
        SavedTemporary3 -> "$s3"
        SavedTemporary4 -> "$s4"
        SavedTemporary5 -> "$s5"
        SavedTemporary6 -> "$s6"
        SavedTemporary7 -> "$s7"
        Temporary8 -> "$t8"
        Temporary9 -> "$t9"
        Kernel0 -> "$k0"
        Kernel1 -> "$k1"
        GlobalPointer -> "$gp"
        StackPointer -> "$sp"
        FramePointer -> "$fp"
        ReturnAddress -> "$ra"

decodeRegister :: Word32 -> Either String Register
decodeRegister = pure . toEnum . fromIntegral
