{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module      :  Main
Copyright   :  (c) Jared Beller 2021
License     :  GPL-3.0-or-later

Stability   :  experimental
Portability :  non-portable (GHC Extensions)

This program implements a basic disassembler for the MIPS architecture.
Argument parsing and basic sequencing happen in this file, while the majority
of parsing and rendering is done in Lib, with help from generic MIPS-related
functions in MIPS.*.
-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Version (showVersion)
import Lib (decodeInstructions, groupSegments, resolveLabels)
import Paths_mips_disasm (version)
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath
import System.IO

data Options = Options
    { optInput :: IO T.Text
    , optOutput :: T.Text -> IO ()
    }

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option ['V'] []
        ( NoArg $ \_ -> do
            progName <- getProgName
            hPutStrLn stderr (progName ++ " version " ++ showVersion version)
            exitSuccess
        )
        "print version"
    , Option ['O'] []
        (NoArg (\opt -> pure opt {optOutput = T.putStr}))
        "output to stdout"
    , Option ['o'] []
        (ReqArg (\arg opt -> pure opt {optOutput = T.writeFile arg}) "")
        "output to a file"
    ]

-- | 'readFile' interprets a file in the platform-native newline format. Since
-- we need to work with text files from both Windows and Unix-like systems,
-- 'universalReadFile' sets the newline mode to universal, which auto-detects
-- the mode a file uses, before reading.
universalReadFile :: FilePath -> IO T.Text
universalReadFile f = do
    file <- openFile f ReadMode
    hSetNewlineMode file universalNewlineMode
    T.hGetContents file

parseArgs :: IO Options
parseArgs = do
    args <- getArgs
    progName <- getProgName
    case getOpt RequireOrder options args of
        (o, n, []) -> case n of
            [] -> applyArgs T.getContents T.putStr o
            [f] -> applyArgs (universalReadFile f) (T.writeFile (f -<.> ".s")) o
            _ -> bail $ usage progName
        (_, _, e) -> bail $ concat [progName, ": ", concat e, usage progName]
  where
    applyArgs input output = foldl (>>=) (pure $ Options input output)
    usage p = "usage: " ++ p ++ " [-VO] [-o output_file] file"
    bail s = hPutStrLn stderr s >> exitFailure

main :: IO ()
main = do
    Options {..} <- parseArgs
    object <- optInput

    case decodeInstructions $ T.lines object of
        Left es -> mapM_ putStrLn es >> exitFailure
        Right rawInstructions -> do
            let (labels, instructions) = resolveLabels rawInstructions
                text = groupSegments labels instructions

            optOutput $ T.intercalate "\n" $ map (T.pack . show) text
