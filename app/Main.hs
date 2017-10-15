module Main where

main :: IO ()
main = do
    putStrLn "Hello, Grass!"

data Instruction =
      App Int Int
    | Abs Int Code

type Code = [Instruction]

data SemanticObject =
      LowerW
    | Out
    | In
    | Succ
    | Closure Code Environment

type Environment = [SemanticObject]

type Dump = [(Code, Environment)]

initEnv :: Environment
initEnv = [ Out, Succ, LowerW, In ]

initDump :: Dump
initDump = [ ([App 1 1], []), ([], []) ]

type MachineConfig = (Code, Environment, Dump)
