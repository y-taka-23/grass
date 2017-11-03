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
    deriving ( Eq, Show )

type Code = [Instruction]

data SemanticObject =
      LowerW
    | Out
    | In
    | Succ
    | Closure Code Environment
    deriving ( Eq, Show )

type Environment = [SemanticObject]

type Dump = [(Code, Environment)]

type MachineConfig = (Code, Environment, Dump)
