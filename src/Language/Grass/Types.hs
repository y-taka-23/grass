module Language.Grass.Types (
      Instruction(..)
    , Code
    , SemanticObject(..)
    , Environment
    , Dump
    , MachineConfig
    ) where

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

type MachineConfig = (Code, Environment, Dump)
