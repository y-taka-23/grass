module Language.Grass.Encoder ( encode, decode ) where

import Language.Grass.Types

import Data.Char ( ord, chr )
import Safe      ( atMay )

boolean :: Bool -> SemanticObject
boolean True  = Closure [Abs 2 [App 3 2]] [Closure [] []]
boolean False = Closure [Abs 2 []] []

encode :: Char -> SemanticObject
encode ch = Closure [Abs 2 (mkCode (ord ch))] []
    where
        mkCode 0 = []
        mkCode n = mkCode (n - 1) ++ [App (n + 1) 1]

decode :: SemanticObject -> Maybe Char
decode LowerW = Just 'w'
decode (Closure [Abs 2 c] _) =
    case evalChurch c [EInt 0, EFunc (+1)] of
        Nothing -> Nothing
        Just i  -> Just $ chr i
decode _ = Nothing

data Evaluable =
      EInt  Int
    | EFunc (Int -> Int)

evalChurch :: Code -> [Evaluable] -> Maybe Int
evalChurch []             ((EInt i):_) = Just i
evalChurch []             _            = Nothing
evalChurch ((Abs _ _):_)  _            = Nothing
evalChurch ((App m n):c') s            =
    case (atMay s (m - 1), atMay s (n - 1)) of
        (Just (EFunc f), Just (EInt i)) -> evalChurch c' ((EInt (f i)):s)
        _                               -> Nothing
