{- |
Module      :  MIPS.ISA.Register
Copyright   :  (c) Jared Beller 2021
License     :  GPL-3.0-or-later
Stability   :  experimental
Portability :  portable
-}
module MIPS.ISA.Register (
      Register(..)

    -- * Binary decoding
    , decodeRegister
) where

import MIPS.ISA.Opcode (Decoder)

data Register
    = Zero
    | AT
    | V0
    | V1
    | A0
    | A1
    | A2
    | A3
    | T0
    | T1
    | T2
    | T3
    | T4
    | T5
    | T6
    | T7
    | S0
    | S1
    | S2
    | S3
    | S4
    | S5
    | S6
    | S7
    | T8
    | T9
    | K0
    | K1
    | GP
    | SP
    | FP
    | RA
    deriving (Enum, Show)

decodeRegister :: Decoder Register
decodeRegister = pure . toEnum . fromEnum
