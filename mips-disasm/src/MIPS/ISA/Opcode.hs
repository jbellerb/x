{-# LANGUAGE DeriveFunctor #-}

-- |
-- Module      :  MIPS.ISA.Opcode
-- Copyright   :  (c) Jared Beller 2021
-- License     :  GPL-3.0-or-later
-- Stability   :  experimental
-- Portability :  portable
module MIPS.ISA.Opcode (
    -- * Opcode decoding
      Decoder
    , runDecoder
) where

import Control.Applicative (Alternative(..))
import Control.Monad (join)
import Data.Word (Word32)

newtype Decoded a = Decoded {unDecoded :: Maybe (Either String a)}
    deriving (Functor)

instance Applicative Decoded where
    pure = Decoded . Just . Right
    x <*> y = x >>= (<$> y)

instance Monad Decoded where
    return = pure

    -- TODO: wtf
    (Decoded x) >>= f = Decoded $ do
        x' <- x
        b <- sequence (unDecoded . f <$> x')
        return $ join b

instance MonadFail Decoded where
    fail = Decoded . Just . Left

instance Alternative Decoded where
    empty = Decoded Nothing
    x <|> m = case unDecoded x of
        Just _ -> x
        Nothing -> m

type Decoder a = Word32 -> Decoded a

runDecoder :: Decoder a -> Word32 -> Either String a
runDecoder decoder w = case unDecoded $ decoder w of
    Just a -> a
    Nothing -> Left "Parser failed to recognise input."
