module Main where

import Data.Char
import Control.Applicative

data JsonValue
    = JsonNull
    | JsonString String
    | JsonInt Int
    | JsonFloat Float
    | JsonBool Bool
    | JsonArray [JsonValue]
    | JsonObject [(String, JsonValue)]
    deriving (Eq, Show)

newtype Parser a = Parser
    { runParser :: String -> Maybe (a, String)
    }

instance Functor Parser where
    fmap f (Parser p) =
        Parser $ \input -> do
            (result, input') <- p input
            Just (f result, input')

instance Applicative Parser where
    pure p = Parser $ \input -> Just (p, input)
    (Parser p1) <*> (Parser p2) =
        Parser $ \input -> do
            (func, input') <- p1 input
            (arg, input'') <- p2 input'
            Just (func arg, input'')

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    (Parser p1) <|> (Parser p2) =
        Parser $ \input -> p1 input <|> p2 input

notEmpty :: Parser [a] -> Parser [a]
notEmpty (Parser p) =
    Parser $ \input -> do
        (result, input') <- p input
        if null result
            then Nothing
            else Just (result, input')

parseChar :: Char -> Parser Char
parseChar ch = Parser f
    where
        f (head:tail)
            | ch == head = Just (ch, tail)
            | otherwise = Nothing
        f [] = Nothing

parseString :: String -> Parser String
parseString = sequenceA . map parseChar

parseSpan :: (Char -> Bool) -> Parser String
parseSpan f = Parser $ \input ->
    let (token, tail) = span f input
    in Just (token, tail)

parseNull :: Parser JsonValue
parseNull = (\_ -> JsonNull) <$> parseString "null"

parseBool :: Parser JsonValue
parseBool = f <$> (parseString "true" <|> parseString "false")
    where f "true"  = JsonBool True
          f "false" = JsonBool False
          f _       = undefined

parseInt :: Parser JsonValue
parseInt = f <$> notEmpty (parseSpan isDigit)
    where f digits = JsonInt $ read digits

parseFloat :: Parser JsonValue
parseFloat = f <$> parseInt <*> parseChar '.' <*> parseInt
    where f (JsonInt i) _ (JsonInt f) = JsonFloat $ fromIntegral i + (fromIntegral f / 10 ^ length (show f))
          f _ _ _ = undefined

stringLiteral :: Parser String
stringLiteral = parseChar '"' *> (parseSpan (/= '"')) <* parseChar '"'

parseStringLiteral :: Parser JsonValue
parseStringLiteral = JsonString <$> stringLiteral

whitespace :: Parser String
whitespace = parseSpan isSpace

separateBy :: Parser a -> Parser b -> Parser [b]
separateBy seperator element = (:) <$> element <*> many (seperator *> element) <|> pure []

parseArray :: Parser JsonValue
parseArray = JsonArray <$> (parseChar '[' *> whitespace *> elements <* whitespace <*parseChar ']')
    where elements = separateBy (whitespace *> parseChar ',' *> whitespace) parseValue

parseObject :: Parser JsonValue
parseObject = JsonObject <$> (parseChar '{' *> whitespace *> separateBy (whitespace *> parseChar ',' <* whitespace) pair <* whitespace <* parseChar '}')
    where pair = (\key _ value -> (key, value)) <$> stringLiteral <*> (whitespace *> parseChar ':' <* whitespace) <*> parseValue

parseValue :: Parser JsonValue
parseValue = parseNull <|> parseBool <|> parseInt <|> parseFloat <|> parseStringLiteral <|> parseArray <|> parseObject

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile fileName parser = do
    input <- readFile fileName
    return (fst <$> runParser parser input)

main :: IO ()
main = undefined