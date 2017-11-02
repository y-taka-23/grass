module Language.Grass.Parser ( parseGrass ) where

import Language.Grass.Types

import           Data.Functor           ( void )
import           Text.Megaparsec        ( some
                                        , char
                                        , many
                                        , noneOf
                                        , (<|>)
                                        , ParseError
                                        , Token
                                        , parse
                                        , Dec
                                        )
import qualified Text.Megaparsec.Lexer  as L
import           Text.Megaparsec.String ( Parser )

parseGrass :: FilePath -> String -> Either (ParseError (Token String) Dec) Code
parseGrass fp = parse codeP fp

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
