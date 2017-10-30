module Main where

import           Control.Exception      ( catch, throwIO )
import           Data.Char              ( ord, chr )
import           Data.Functor           ( void )
import           Safe                   ( atMay )
import           System.IO.Error        ( isEOFError )
import           Text.Megaparsec        ( some, char, many, noneOf, (<|>) )
import qualified Text.Megaparsec.Lexer  as L
import           Text.Megaparsec.String ( Parser )

main :: IO ()
main = do
    putStrLn "Hello, Grass!"

ignoredP :: Parser Char
ignoredP = noneOf "WwvＷｗｖ"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme $ void $ many ignoredP

upperWP :: Parser Char
upperWP = lexeme $ char 'W' <|> char 'Ｗ'

lowerWP :: Parser Char
lowerWP = lexeme $ char 'w' <|> char 'ｗ'

lowerVP :: Parser Char
lowerVP = lexeme $ char 'v' <|> char 'ｖ'

appP :: Parser Instruction
appP = do
    funcIdx <- some upperWP
    argIdx  <- some lowerWP
    return $ App (length funcIdx) (length argIdx)

absP :: Parser Instruction
absP = do
    argNum <- some lowerWP
    body   <- many appP
    return $ Abs (length argNum) body

codeP :: Parser Code
codeP = do
    _    <- many ignoredP
    abs  <- absP
    rest <- restCodeP
    return $ abs : rest

restCodeP :: Parser Code
restCodeP = restAbsP <|> restAppsP <|> mempty
    where
        restAbsP = do
            _   <- lowerVP
            abs <- absP
            return [abs]
        restAppsP = do
            _    <- lowerVP
            apps <- many appP
            return apps

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
    let (func, arg) = (env !! (m - 1), env !! (n - 1))
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

run :: Code -> IO SemanticObject
run initCode = eval (initCode, initEnv, initDump)
    where
        eval :: MachineConfig -> IO SemanticObject
        eval ([], [obj], [])   = return obj
        eval (code, env, dump) =
            transform (code, env, dump) >>= eval
