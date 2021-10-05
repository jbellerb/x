{-# LANGUAGE PartialTypeSignatures #-}

module Lib where

import Data.Word (Word32)
import MIPS.ISA.Instruction (Instruction, decodeInstruction)
import MIPS.ISA.Opcode (runDecoder)

-- Usually this would be done with the traverse function, however Either's
-- Applicative instance only returns the first error, so we have to create
-- our own traverse which collects multiple errors.
decodeInstructions :: [Word32] -> Either [String] [Instruction]
decodeInstructions = foldr process (pure []) . zip [1 :: Int ..]
  where
    process (i, word) ys = case runDecoder decodeInstruction word of
        Right a -> Right (a :) <*> ys
        Left a -> Left $ case ys of
            Right _ -> ["Line " ++ show i ++ ": " ++ a]
            Left es -> ("Line " ++ show i ++ ": " ++ a) : es
