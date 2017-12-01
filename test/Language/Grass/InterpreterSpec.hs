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
            prop "makes Out output the arg to stdout" prop_pout
            prop "makes In stack the head of stdin against Env" prop_pin
            prop "makes Succ increment the top of Env in mod 255" prop_psucc
            prop "makes a char test its equality with the arg" prop_pchar

        context "when you evalutate Church numbers" $ do
            prop "computes Church encoding as iteration" prop_iter
            prop "computes the sum of two numbers" prop_plus
            prop "copputes the product of two numbers" prop_mult
            prop "computes the exponential of the 1st arg to the 2nd" prop_pow
            prop "computes the successor of the arg" prop_succ

        context "when you evaluate Church booleans" $ do
            prop "handles the true constant" prop_true
            prop "handles the false constant" prop_false
            prop "handles the and operater" prop_and
            prop "handles the or operater" prop_or
            prop "handles the not operater" prop_not
            prop "tests whether the given number equals zero" prop_iszero

        context "when you evaluate recursive functions" $ do
            prop "loops the main function infinitely" prop_infinite

prop_pout :: Char -> Bool
prop_pout ch = execMock (code, env, initDump) [] == [ch]
    where
        code = [Abs 1 [App 3 2]]
        env  = [Character ch, Out]

prop_pin :: Char -> Bool
prop_pin ch = execMock (code, env, initDump) [ch] == [ch]
    where
        code = [Abs 1 [App 2 2, App 4 1]]
        env  = [In, Out]

prop_psucc :: Char -> Bool
prop_psucc ch = ord res == (ord ch + 1) `mod` 255
    where
        code  = [Abs 1 [App 3 2, App 5 1]]
        env   = [Character ch, Succ, Out]
        [res] = execMock (code, env, initDump) []

prop_pchar :: Char -> Char -> Bool
prop_pchar ch1 ch2 =
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
prop_iter ch (NonNegative n) =
    execMock (code, env, initDump) [] == replicate n ch
        where
            code = [Abs 1 [App 2 4, App 1 4]]
            env  = [encode n, Character ch, Out]

-- λn m f x. n f (m f x)
prop_plus :: Char -> NonNegative Int -> NonNegative Int -> Bool
prop_plus ch (NonNegative n) (NonNegative m) =
    execMock (code, env, initDump) [] == replicate (n + m) ch
        where
            code = [
                  Abs 4 [App 4 2, App 4 3, App 1 3, App 3 1]
                , Abs 1 [App 2 4, App 1 4, App 1 8, App 1 8]
                ]
            env  = [encode m, encode n, Character ch, Out]

-- λn m f. n (m f)
prop_mult :: Char -> NonNegative Int -> NonNegative Int -> Bool
prop_mult ch (NonNegative n) (NonNegative m) =
    execMock (code, env, initDump) [] == replicate (n * m) ch
        where
            code = [
                  Abs 3 [App 2 1, App 4 1]
                , Abs 1 [App 2 4, App 1 4, App 1 8, App 1 8]
                ]
            env  = [encode m, encode n, Character ch, Out]

-- λn m. m n
prop_pow :: Char -> NonNegative Int -> Bool
prop_pow ch (NonNegative n) =
    execMock (code, env, initDump) [] == replicate (n ^ 2) ch
        where
            code = [
                  Abs 2 [App 1 2]
                , Abs 1 [App 2 4, App 1 4, App 1 8, App 1 8]
                ]
            env  = [encode 2, encode n, Character ch, Out]

-- λn f x. f (n f x)
prop_succ :: Char -> NonNegative Int -> Bool
prop_succ ch (NonNegative n) =
    execMock (code, env, initDump) [] == replicate (n + 1) ch
        where
            code = [
                  Abs 3 [App 3 2, App 1 2, App 4 1]
                , Abs 1 [App 2 3, App 1 6, App 1 6]
                ]
            env = [encode n, Character ch, Out]

prop_true :: Char -> Char -> Bool
prop_true ch1 ch2 = execMock (code, env, initDump) [] == [ch1]
    where
        code = [Abs 1 [App 2 4, App 1 4, App 7 1]]
        env  = [boolean True, Character ch2, Character ch1, Out]

prop_false :: Char -> Char -> Bool
prop_false ch1 ch2 = execMock (code, env, initDump) [] == [ch2]
    where
        code = [Abs 1 [App 2 4, App 1 4, App 7 1]]
        env  = [boolean False, Character ch2, Character ch1, Out]

-- λp q. p q p
prop_and :: Char -> Char -> Bool -> Bool -> Bool
prop_and ch1 ch2 b1 b2 =
    execMock (code, env, initDump) [] == if b1 && b2 then [ch1] else [ch2]
        where
            code = [
                  Abs 2 [App 2 1, App 1 3]
                , Abs 1 [App 2 4, App 1 4, App 1 8, App 1 8, App 11 1]
                ]
            env  = [boolean b2, boolean b1, Character ch2, Character ch1, Out]

-- λp q. p p q
prop_or :: Char -> Char -> Bool -> Bool -> Bool
prop_or ch1 ch2 b1 b2 =
    execMock (code, env, initDump) [] == if b1 || b2 then [ch1] else [ch2]
        where
            code = [
                  Abs 2 [App 2 2, App 1 2]
                , Abs 1 [App 2 4, App 1 4, App 1 8, App 1 8, App 11 1]
                ]
            env  = [boolean b2, boolean b1, Character ch2, Character ch1, Out]

-- λp. p FALSE TRUE
prop_not :: Char -> Char -> Bool -> Bool
prop_not ch1 ch2 b =
    execMock (code, env, initDump) [] == if not b then [ch1] else [ch2]
        where
            code = [
                  Abs 1 [App 1 4, App 1 4]
                , Abs 1 [App 2 3, App 1 8, App 1 8, App 11 1]
                ]
            env  = [
                  boolean b
                , boolean True
                , boolean False
                , Character ch2
                , Character ch1
                , Out
                ]

-- λn. n (λx. FALSE) TRUE
prop_iszero :: Char -> Char -> NonNegative Int -> Bool
prop_iszero ch1 ch2 (NonNegative n) =
    execMock (code, env, initDump) [] == if n == 0 then [ch1] else [ch2]
        where
            code = [
                  Abs 1 []
                , Abs 1 [App 2 5]
                , Abs 1 [App 1 2, App 1 6]
                , Abs 1 [App 2 5, App 1 10, App 1 10, App 13 1]
                ]
            env  = [
                  encode n
                , boolean True
                , boolean False
                , Character ch2
                , Character ch1
                , Out
                ]

prop_infinite :: Char -> NonNegative Int -> Bool
prop_infinite ch (NonNegative n) =
    take n (execMock (code, env, initDump) []) == replicate n ch
        where
            code = [Abs 1 [App 3 2, App 2 2]]
            env  = [Character ch, Out]
