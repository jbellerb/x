{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  MIPS.Assembly
Copyright   :  (c) Jared Beller 2021
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  non-portable (GHC Extensions)
-}

module MIPS.Assembly
    ( -- * Dynamic instruction types
      UnresolvedInstruction (..)
    , ResolvedInstruction (..)

      -- * Program segment types
    , Segment (..)
    , Label (..)

      -- * Helper functions
    , makeAnonymousLabel
    ) where

import Data.Bits (shift, (.&.))
import qualified Data.Text as T
import Data.Word (Word32)
import MIPS.ISA.Instruction (Instruction (..))
import Numeric (showHex)

data UnresolvedInstruction
    = UnresolvedBranch Instruction Label
    | UnresolvedStatic Instruction

newtype ResolvedInstruction = ResolvedInstruction Instruction

data Segment
    = BaseSegment
        { segmentCode :: [UnresolvedInstruction]
        }
    | Segment
        { segmentLabel :: Label
        , segmentCode :: [UnresolvedInstruction]
        }

data Label = Label
    { labelLocation :: Word32
    , labelName :: T.Text
    }
    deriving (Eq, Ord)

instance Show UnresolvedInstruction where
    show op = case op of
        UnresolvedBranch (BranchEqual rt rs _) l -> showLabeled "beq" rt rs l
        UnresolvedBranch (BranchNotEqual rt rs _) l -> showLabeled "bne" rt rs l
        UnresolvedBranch a _ -> show a
        UnresolvedStatic a -> show a
      where
        showLabeled name a b l =
            concat [name, " ", show a, ", ", show b, ", ", T.unpack $ labelName l]

instance Show ResolvedInstruction where
    show (ResolvedInstruction a) = show a

instance Show Segment where
    show segment = case segment of
        BaseSegment {..} -> indent segmentCode
        Segment {..} -> show segmentLabel ++ ":\n" ++ indent segmentCode
      where
        indent code = unlines $ map (("    " ++) . show) code

instance Show Label where
    show Label {..} = T.unpack labelName

makeAnonymousLabel :: Word32 -> Label
makeAnonymousLabel location = Label location $ makeName location
  where
    makeName = T.append "Addr_" . T.pack . showHex4
    showHex4 w = snd $ foldr addHexDigit (w, "") [0 :: Int .. 3]
    addHexDigit _ (w', s) = (shift w' (-4), showHex (w' .&. 0xf) s)
