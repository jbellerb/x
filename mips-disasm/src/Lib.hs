{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Data.Text as T
import Data.Text.Read (hexadecimal)
import Data.Word (Word32)
import MIPS.ISA.Instruction (Instruction, decodeInstruction)

parseHex :: T.Text -> Either String Word32
parseHex t = case hexadecimal t of
    Right (a, "") -> pure a
    _ -> Left $ "Invalid hex \"" ++ T.unpack t ++ "\""

-- Usually this would be done with the traverse function, however Either's
-- Applicative instance only returns the first error, so we have to create
-- our own traverse which collects multiple errors.
decodeInstructions :: [T.Text] -> Either [String] [Instruction Word32]
decodeInstructions = foldr process (pure []) . zip [1 :: Int ..]
  where
    errorMessage e i = "Line " ++ show i ++ ": " ++ e
    process (i, word) ys = case decodeInstruction =<< parseHex word of
        Right a -> (a :) <$> ys
        Left e -> Left $ case ys of
            Right _ -> [errorMessage e i]
            Left es -> errorMessage e i : es
