{-# LANGUAGE OverloadedStrings #-}

{- |
Module      :  Lib
Copyright   :  (c) Jared Beller 2021
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  non-portable (GHC Extensions)

This module contains various helper functions related to the disassembly
process, including a master parser and functions for resolving labels and
groups. These functions are meant to be called directly by Main, and are
application-specific, so I didn't think they belonged in MIPS.*.
-}

module Lib
    ( decodeInstructions
    , resolveLabels
    , groupSegments
    ) where

import qualified Data.Set as S
import qualified Data.Text as T
import Data.Text.Read (hexadecimal)
import Data.Word (Word32)
import MIPS.Assembly
import MIPS.ISA.Instruction (Instruction (..), decodeInstruction)

parseHex :: T.Text -> Either String Word32
parseHex t = case hexadecimal t of
    Right (a, "") -> pure a
    _ -> Left $ "Invalid hex \"" ++ T.unpack t ++ "\""

-- Usually this would be done with the traverse function, however Either's
-- Applicative instance only returns the first error, so we have to create
-- our own traverse which collects multiple errors.
decodeInstructions :: [T.Text] -> Either [String] [ResolvedInstruction]
decodeInstructions = foldr process (pure []) . zip [1 :: Int ..]
  where
    errorMessage w i = "Cannot disassemble " ++ T.unpack w ++ " at line " ++ show i
    process (i, word) ys = case decodeInstruction =<< parseHex word of
        Right a -> (ResolvedInstruction a :) <$> ys
        Left _ -> Left $ case ys of
            Right _ -> [errorMessage word i]
            Left es -> errorMessage word i : es

resolveLabels :: [ResolvedInstruction] -> (S.Set Label, [UnresolvedInstruction])
resolveLabels = foldr resolveBranch (S.empty, []) . zip [1 ..]

resolveBranch ::
    (Word32, ResolvedInstruction) ->
    (S.Set Label, [UnresolvedInstruction]) ->
    (S.Set Label, [UnresolvedInstruction])
resolveBranch (i, ResolvedInstruction op) (s, ops) = case op of
    BranchEqual _ _ offset -> makeBranch op ops $ makeLabel offset
    BranchNotEqual _ _ offset -> makeBranch op ops $ makeLabel offset
    _ -> (s, UnresolvedStatic op : ops)
  where
    makeLabel o = makeAnonymousLabel $ 4 * (i + fromIntegral o)
    makeBranch op' ops' l = (S.insert l s, UnresolvedBranch op' l : ops')

groupSegments :: S.Set Label -> [UnresolvedInstruction] -> [Segment]
groupSegments labels instructions = case namedSegments of
    (segments, []) -> segments
    (segments, remaining) -> BaseSegment remaining : segments
  where
    namedSegments :: ([Segment], [UnresolvedInstruction])
    namedSegments = foldr nextSegment ([], instructions) $ S.toAscList labels

nextSegment ::
    Label ->
    ([Segment], [UnresolvedInstruction]) ->
    ([Segment], [UnresolvedInstruction])
nextSegment label (segments, remaining) =
    (Segment label code : segments, remaining')
  where
    (remaining', code) =
        splitAt (fromIntegral (labelLocation label `div` 4)) remaining
