{-# LANGUAGE DeriveFunctor #-}
module Language.Grass.Interpreter ( exec ) where

import Language.Grass.Encoder
import Language.Grass.Types

import Control.Exception  ( catch, throwIO )
import Control.Monad.Free ( Free(..), liftF )
import Data.Char          ( ord, chr )
import Safe               ( atMay )
import System.IO.Error    ( isEOFError )

data GrassActionF a =
      PutChar Char a
    | GetChar (Maybe Char -> a)
    deriving ( Functor )

type GrassAction = Free GrassActionF

putCharAction :: Char -> GrassAction ()
putCharAction ch = liftF $ PutChar ch ()

getCharAction :: GrassAction (Maybe Char)
getCharAction = liftF $ GetChar id

succMod255 :: Char -> Char
succMod255 ch = chr $ (ord ch + 1) `mod` 255

transform :: MachineConfig -> GrassAction MachineConfig
transform ((App m n):c, e, d) = case (atMay e (m - 1), atMay e (n - 1)) of
    (Nothing, _      ) -> error "invalid stack access"
    (_,       Nothing) -> error "invalid stack access"
    (Just f,  Just x ) -> case f of
        LowerW -> pure (c, (boolean $ decode x == Just 'w'):e, d)
        Out -> case decode x of
            Nothing -> error "failed to decode"
            Just ch -> putCharAction ch >> pure (c, x:e, d)
        In -> getCharAction >>= \mCh -> case mCh of
            Nothing -> pure (c, x:e, d)
            Just ch -> pure (c, (encode ch):e, d)
        Succ -> case decode x of
            Nothing -> error "failed to decode"
            Just ch -> pure (c, (encode $ succMod255 ch):e, d)
        Closure c' e' -> pure (c', x:e', (c, e):d)
transform ((Abs 1 c'):c, e, d)  = pure (c, (Closure c' e):e, d)
transform ((Abs n c'):c, e, d)  = pure (c, (Closure [Abs (n-1) c'] e):e, d)
transform ([], o:_, (c', e'):d) = pure (c', o:e', d)
transform _                     = error "no transformation rules"

eval :: MachineConfig -> GrassAction ()
eval ([], [_], []) = return ()
eval (c, e, d)     = transform (c, e, d) >>= eval

getCharOrEOF :: IO (Maybe Char)
getCharOrEOF =
    (do
            getChar >>= return . Just
        ) `catch` (\e -> do
            if isEOFError e
                then return Nothing
                else throwIO e
        )

interpret :: GrassAction a -> IO a
interpret (Pure x)              = return x
interpret (Free (PutChar ch k)) = putChar ch >> interpret k
interpret (Free (GetChar f))    = getCharOrEOF >>= interpret . f

initEnv :: Environment
initEnv = [ Out, Succ, LowerW, In ]

initDump :: Dump
initDump = [ ([App 1 1], []), ([], []) ]

exec :: Code -> IO ()
exec initCode = interpret $ eval (initCode, initEnv, initDump)
