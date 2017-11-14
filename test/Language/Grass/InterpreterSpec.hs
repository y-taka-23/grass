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

encode :: Int -> SemanticObject
encode n = Closure [Abs 1 body] []
    where
        body = map (\i -> App i 1) [2 .. n + 1]

spec :: Spec
spec = do
    describe "eval" $ do

        context "when you eavluate primitives" $ do
            prop "makes Out output the arg to stdout" prop_out
            prop "makes In stack the head of stdin against Env" prop_in
            prop "makes Succ increment the top of Env in mod 255" prop_succ
            prop "makes a char test its equality with the arg" prop_char

        context "when you evalutate Church numbers" $ do
            prop "computes Church encoding as iteration" prop_iter
            prop "computes the sum of two numbers" prop_plus
            prop "copputes the product of two numbers" prop_mult
            prop "computes the exponential of the 1st arg to the 2nd" prop_pow

        context "when you evaluate recursive functions" $ do
            prop "loops the main function infinitely" prop_infinite

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

prop_iter :: Char -> NonNegative Int -> Bool
prop_iter ch nnn = execMock (code, env, initDump) [] == replicate n ch
    where
        n    = getNonNegative nnn
        code = [Abs 1 [App 2 4, App 1 4]]
        env  = [encode n, Character ch, Out]

-- λn m f x. n f (m f x)
prop_plus :: Char -> NonNegative Int -> NonNegative Int -> Bool
prop_plus ch nnn nnm =
    execMock (code, env, initDump) [] == replicate (n + m) ch
        where
            (n, m) = (getNonNegative nnn, getNonNegative nnm)
            code   = [
                  Abs 4 [App 4 2, App 4 3, App 1 3, App 3 1]
                , Abs 1 [App 2 4, App 1 4, App 1 8, App 1 8]
                ]
            env    = [encode m, encode n, Character ch, Out]

-- λn m f. n (m f)
prop_mult :: Char -> NonNegative Int -> NonNegative Int -> Bool
prop_mult ch nnn nnm =
    execMock (code , env, initDump) [] == replicate (n * m) ch
        where
            (n, m) = (getNonNegative nnn, getNonNegative nnm)
            code   = [
                  Abs 3 [App 2 1, App 4 1]
                , Abs 1 [App 2 4, App 1 4, App 1 8, App 1 8]
                ]
            env    = [encode m, encode n, Character ch, Out]

-- λn m. m n
prop_pow :: Char -> NonNegative Int -> Bool
prop_pow ch nnn =
    execMock (code , env, initDump) [] == replicate (n ^ 2) ch
        where
            n    = getNonNegative nnn
            code = [
                  Abs 2 [App 1 2]
                , Abs 1 [App 2 4, App 1 4, App 1 8, App 1 8]
                ]
            env  = [encode 2, encode n, Character ch, Out]

prop_infinite :: Char -> NonNegative Int -> Bool
prop_infinite ch nnn =
    take n (execMock (code, env, initDump) []) == replicate n ch
        where
            n    = getNonNegative nnn
            code = [Abs 1 [App 3 2, App 2 2]]
            env  = [Character ch, Out]
