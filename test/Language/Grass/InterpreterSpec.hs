module Language.Grass.InterpreterSpec where

import Language.Grass.Interpreter
import Language.Grass.Types

import Control.Monad.Free    ( Free(..) )
import Data.Char             ( ord )
import Test.Hspec
import Test.Hspec.QuickCheck ( prop )
import Test.QuickCheck

mock :: GrassAction a -> [Char] -> [Char]
mock (Pure x)              _           = []
mock (Free (PutChar ch k)) stdin       = ch : mock k stdin
mock (Free (GetChar f))    []          = mock (f Nothing) []
mock (Free (GetChar f))    (ch:stdin') = mock (f (Just ch)) stdin'

execMock :: MachineConfig -> [Char] -> [Char]
execMock cfg stdin = mock (eval cfg) stdin

spec :: Spec
spec = do
    describe "eval" $ do

        context "when you eavluate primitives" $ do
            prop "makes Out output the arg to stdout" prop_out
            prop "makes In stack the head of stdin against Env" prop_in
            prop "makes Succ increment the top of Env in mod 255" prop_succ
            prop "makes a char test its equality with the arg" prop_char

prop_out :: Char -> Bool
prop_out ch = execMock (code, env, initDump) [] == [ch]
    where
        code = [Abs 1 [App 3 2]]
        env  = [Character ch, Out]

prop_in :: Char -> Bool
prop_in ch = execMock (code, env, initDump) [ch] == [ch]
    where
        code = [Abs 1 [App 2 2, App 4 1]]
        env  = [In, Out]

prop_succ :: Char -> Bool
prop_succ ch = ord res == (ord ch + 1) `mod` 255
    where
        code  = [Abs 1 [App 3 2, App 5 1]]
        env   = [Character ch, Succ, Out]
        [res] = execMock (code, env, initDump) []

prop_char :: Char -> Char -> Bool
prop_char ch1 ch2 =
    execMock (code, env, initDump) [] == if ch1 == ch2 then ['Y'] else ['N']
        where
            code  = [Abs 1 [App 2 3, App 1 6, App 1 6, App 9 1]]
            env   = [
                  Character ch1
                , Character ch2
                , Character 'N'
                , Character 'Y'
                , Out
                ]
