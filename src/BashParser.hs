{-# LANGUAGE OverloadedStrings #-}


module BashParser where

import Data.Text as T
import Control.Applicative
import Data.Map
import Data.Attoparsec.Text as P
import Data.Char


newtype QuotedChar = BS Char
  deriving (Eq, Show)
data QuotedText = SQ Text | DQ Text
  deriving (Eq, Show)


data Token
  = Identifier Text
  | ControlOperator Text





blank = satisfy isHorizontalSpace


isNameStart c = isLower c || isUpper c || c == '_'
isNameChars c = isLower c || isUpper c || c == '_' || isDigit c

identifier = T.cons <$> P.satisfy isNameStart <*> P.takeWhile isNameChars

controlOperator = string "||" <|>
                  string "&&" <|>
                  string "&" <|>
                  string ";" <|>
                  string ";;" <|>
                  string "(" <|>
                  string ")" <|>
                  string "|" <|>
                  string "|&" <|>
                  string "\n"

word = identifier

isWordSeparator c = isPipe c ||
                    isAmp c ||
                    isSemicolon c ||
                    isLeftParen c ||
                    isRightParen c ||
                    isLt c ||
                    isGt c ||
                    isHorizontalSpace c

isPipe = (=='|')
isAmp = (=='&')
isSemicolon = (==';')
isLeftParen = (=='(')
isRightParen = (==')')
isLt = (=='<') 
isGt = (=='>')



quotedChar :: Parser Char -> Parser QuotedChar
quotedChar chr = backSlashedChar
  where
    backSlashedChar = char '\\' *> fmap BS chr



-- >>> parseOnly (unquotedChar anyChar) "\texxt"

unquotedChar :: Parser Char -> Parser Char
-- unquotedChar = undefined
unquotedChar chr = do
  q <- peekChar'
  case q of
    '\\' -> fail ""
    _    -> chr

-- unquotedChar :: Parser Char -> Parser Char
-- unquotedChar chr = singleQuotedChar <|> doubleQuotedChar
--   where
--     singleQuotedChar = char '\'' *> fmap SQC chr <* char '\''
--     doubleQuotedChar = char '"' *> fmap DQC chr <* char '"'


data SimpleCommand = SimpleCommand
  { vars :: Map Text Text
  , cmd :: Text
  , args :: [Text]
  , redirects :: [Text]
  , ctrlOper :: Text
  }

variableAssignment :: Parser (Text,Text)
variableAssignment = do
  n <- identifier
  _ <- char '='
  w <- word
  return (n,w)



-- readSimpleCommand :: Parser SimpleCommand
-- readSimpleCommand = do
--   vas <- many' variableAssignment
  
