module Parser
    ( vhdlParser
    ) where

import Types

import Data.Char (toUpper, toLower)
import Data.Maybe (fromMaybe)
import Control.Monad (replicateM)
import Text.Parsec.Text
import Text.Parsec
import Control.Applicative hiding ((<|>), many, optional)

{- case-insensitive parsers taken from https://stackoverflow.com/users/507803/heatsink's answer on
   https://stackoverflow.com/questions/12937325/whats-the-cleanest-way-to-do-case-insensitive-parsing-with-text-combinators-par 

   I took the liberty of renaming them to char' and string' from the more verbose `caseInsensitive[Char|String]`. Since VHDL itself is
   case insensitive I felt it unnecessary to spell out (at least as explicitly) where we're using insensitive parsers--in this 
   implementation should just be anything reliant on matching VHDL keywords
-}
char' :: Char -> Parser Char
char' c = char (toLower c) <|> char (toUpper c)

string' :: String -> Parser String
string' s = try $ mapM char' s

{- | 'Paren' is a  helper data type for parsing arbitrarily 
     nested paren-wrapped vector definitions & default values
 -}
data Paren = Inside String | Wrapped [Paren]

parens :: Parser String
parens = collect <$> wrapped
    where
        collect :: Paren -> String
        collect (Inside str)   = str
        collect (Wrapped pars) = "(" ++ concatMap collect pars ++ ")"

        parseParens :: Parser Paren
        parseParens = inside <|> wrapped

        inside :: Parser Paren
        inside  = Inside <$> many1 (noneOf "()")

        wrapped :: Parser Paren
        wrapped = Wrapped <$> between (char '(') (char ')') (many parseParens)


{- | 'vhdlParser' parses a plain text VHDL file into the psuedo AST
     `VhdlFile` such that it can be easily used by pure Haskell 
     functions downstream.
 -}
vhdlParser :: Parser VhdlFile
vhdlParser = VhdlFile <$> entityName <*> generics <*> ports
    where
        entityName :: Parser String
        entityName =  manyTill (comment <|> code) (try $ string' "entity")
                   *> whitespace
                   *> anyStringTill space
                   <* whitespace
                   <* string' "is"

        generics :: Parser (Maybe [Generic])
        generics = optionalParse $ parseParameters "generic" generic
            where
                generic :: Parser Generic
                generic = Generic <$> portNames
                                  <*> portType
                                  <*> portValue

        ports :: Parser [Port]
        ports = parseParameters "port" port
            where
                port :: Parser Port
                port = Port <$> portNames
                            <*> portDirection
                            <*> portType
                            <*> portValue

        anyStringTill :: Parser a -> Parser String
        anyStringTill = manyTill anyChar

        whitespace :: Parser ()
        whitespace = skipMany space

        newline :: Parser String
        newline = many1 $ oneOf "\r\n"

        comment :: Parser String
        comment = spaces *> string "--" *> manyTill anyChar newline <* spaces

        code :: Parser String
        code = spaces *> manyTill anyChar newline <* spaces

        comments :: Parser ()
        comments = skipMany comment

        optionalParse :: Parser a -> Parser (Maybe a)
        optionalParse p = optionMaybe $ try p

        parseParameters :: String -> Parser a -> Parser [a]
        parseParameters str paramParser =  anyStringTill (string' str)
                                        *> anyStringTill (char '(')
                                        *> whitespace
                                        *> sepBy1 paramParser' paramSep
                                        <* anyStringTill (string ");")
            where
                paramParser' = whitespace *> comments *> paramParser

                paramSep :: Parser ()
                paramSep = char ';' *> optional newline

        portNames :: Parser [String]
        portNames = spaces *> sepEndBy1 name separator <* char ':'
            where
                name :: Parser String
                name = many1 (alphaNum <|> char '_')

                separator :: Parser String
                separator = many1 (oneOf ", \t")
        
        portDirection :: Parser String
        portDirection = spaces *> many1 alphaNum <* spaces

        portType :: Parser String
        portType = spaces *> basicType <> vector <* spaces
            where
                basicType :: Parser String
                basicType = many1 (alphaNum <|> char '_')

                vector :: Parser String
                vector = option "" parens

        portValue :: Parser (Maybe String)
        portValue = optionalParse $ spaces *> string ":=" *> spaces *> (parens <|> value)
            where
                value :: Parser String
                value = many (alphaNum <|> oneOf " '_*+-/")
