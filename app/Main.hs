module Main where

import Control.Exception ( catch, throwIO )
import Data.Char         ( ord, chr )

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

decodeChurch :: SemanticObject -> Maybe Char
decodeChurch = undefined

encodeChurch :: Char -> Maybe SemanticObject
encodeChurch = undefined

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
        LowerW -> undefined
        Out -> case decodeChurch arg of
            Nothing -> error "failed to decode"
            Just c  -> putChar c >> return (code, arg : env, dump)
        In -> do
            mChar <- safeGetChar
            case mChar of
                Nothing -> return (code, arg : env, dump)
                Just c  -> case encodeChurch c of
                    Nothing  -> error "failed to decode"
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
