module Main where

import Control.Exception ( catch, throwIO )
import Data.Char         ( ord, chr )
import Safe              ( atMay )
import System.IO.Error   ( isEOFError )

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

churchTrue :: SemanticObject
churchTrue = Closure [Abs 2 [App 3 2]] [Closure [] []]

churchFalse :: SemanticObject
churchFalse = Closure [Abs 2 []] []

decodeChurch :: SemanticObject -> Maybe Char
decodeChurch LowerW = Just 'w'
decodeChurch Out    = Nothing
decodeChurch In     = Nothing
decodeChurch Succ   = Nothing
decodeChurch (Closure [Abs 2 code] env) =
    case evalChurch code [ElemInt 0, ElemFunc succ] of
        Nothing -> Nothing
        Just i  -> if isGrassChar (chr i) then Just (chr i) else Nothing

data StackElem =
      ElemInt  Int
    | ElemFunc (Int -> Int)

evalChurch :: Code -> [StackElem] -> Maybe Int
evalChurch [] (ElemInt i : stack') = Just i
evalChurch [] _                    = Nothing
evalChurch (Abs _ _ : _) _         = Nothing
evalChurch (App m n : code') stack =
    case (atMay stack (m - 1), atMay stack (n - 1)) of
        (Just (ElemFunc f), Just (ElemInt i)) -> evalChurch code' (ElemInt (f i) : stack)
        (_,                 _               ) -> Nothing

encodeChurch :: Char -> Maybe SemanticObject
encodeChurch c
    | isGrassChar c = Just $ Closure [Abs 2 (mkCode n)] []
    | otherwise     = Nothing
    where
        n        = ord c
        mkCode 0 = []
        mkCode n = mkCode (n - 1) ++ [App (n + 1) 1]

isGrassChar :: Char -> Bool
isGrassChar c = let n = ord c in 0 <= n && n <= 255

succCode :: Char -> Char
succCode c = chr $ ord c + 1 `mod` 255

safeGetChar :: IO (Maybe Char)
safeGetChar =
    (do
            getChar >>= return . Just
        ) `catch` (\e -> do
            if isEOFError e
                then return Nothing
                else throwIO e
        )

transform :: MachineConfig -> IO MachineConfig
transform (App m n : code, env, dump) =
    let (func, arg) = (env !! m, env !! n)
    in case func of
        LowerW -> if decodeChurch LowerW == Just 'w'
            then return (code, churchTrue  : env, dump)
            else return (code, churchFalse : env, dump)
        Out -> case decodeChurch arg of
            Nothing -> error "failed to decode"
            Just c  -> putChar c >> return (code, arg : env, dump)
        In -> do
            mChar <- safeGetChar
            case mChar of
                Nothing -> return (code, arg : env, dump)
                Just c  -> case encodeChurch c of
                    Nothing  -> error "failed to encode"
                    Just res -> return (code, res : env, dump)
        Succ -> case decodeChurch arg of
            Nothing -> error "failed to decode"
            Just c  -> case encodeChurch (succCode c) of
                Nothing  -> error "unreachable"
                Just res -> return (code, res : env, dump)
        Closure fCode fEnv -> return (fCode, arg : fEnv, (code, env) : dump)
transform (Abs 1 code' : code, env, dump) = return (code, Closure code' env : env, dump)
transform (Abs n code' : code, env, dump) = return (code, Closure [Abs (n - 1) code'] env : env, dump)
transform ([], obj : _, (code', env') : dump) = return (code', obj : env', dump)
