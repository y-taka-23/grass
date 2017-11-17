module Main where

import Language.Grass.Interpreter
import Language.Grass.Parser

import System.Environment ( getArgs )
import System.Exit        ( die )

main :: IO ()
main = do
    fp  <- head <$> getArgs
    src <- readFile fp
    case parseGrass fp src of
        Left  err  -> die $ show err
        Right code -> exec code
