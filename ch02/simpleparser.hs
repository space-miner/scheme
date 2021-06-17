module Main where
import Control.Monad
import Numeric
import Data.Char(digitToInt)
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

main :: IO ()
main = do args <- getArgs
          putStrLn $ readExpr ( args !! 0)

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value " ++ show val

type Numerator = Integer
type Denominator = Integer

data LispVal = Atom String 
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer 
             | Float Float
             | Rational (Numerator, Denominator)
             | String String 
             | Char Char
             | Bool Bool deriving (Show)

escapedChars :: Parser Char
escapedChars = do char '\\'
                  x <- oneOf ['\\','\"','n','t','r']
                  case x of
                     '\\' -> return '\\'
                     '"' -> return '\"'
                     'n' -> return '\n'
                     't' -> return '\t'
                     'r' -> return '\r'

parseChar :: Parser LispVal
parseChar = do try (string "#\\")
               x <- many1 letter
               return $ case x of
                  "space" -> Char ' '
                  "newline" -> Char '\n'
                  c:_ -> Char c

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ escapedChars <|> (noneOf ['\\','"'])
                 char '"'
                 return $ String x

parseDec :: Parser LispVal
parseDec = do try (string "#d")
              x <- many1 (oneOf "0123456789")
              return $ Number (read x)

readBin :: String -> Integer
readBin xs = foldl (\acc x-> (2 * acc) + (toInteger $ digitToInt x)) 0 xs

parseBin :: Parser LispVal
parseBin = do try (string "#b")
              x <- many (oneOf "01")
              return $ Number (readBin x)

parseOct :: Parser LispVal
parseOct = do try (string "#o")
              x <- many1 (oneOf "01234567")
              return $ Number (fst $ (readOct x) !! 0)

parseHex :: Parser LispVal
parseHex = do try (string "#x")
              x <- many1 (oneOf "0123456789abcdefABCDEF")
              return $ Number (fst $ (readHex x) !! 0)

parseRadix :: Parser LispVal
parseRadix = try parseDec 
         <|> try parseBin
         <|> try parseOct 
         <|> try parseHex 
         <|> parseAtom

parseNum :: Parser LispVal
parseNum = many1 digit >>= \n ->
           return $ Number (read n)

parseNumber :: Parser LispVal
parseNumber = parseRadix <|> parseNum
{-- parseNumber = do n <- many1 digit
                 return $ Number (read n) --}
-- parseNumber = liftM (Number . read) $ many1 digit

parseFloat :: Parser LispVal
parseFloat = do whole <- many digit
                char '.' 
                frac <- many digit
                return $ Float (read $ whole ++ "." ++ frac :: Float)

parseRational :: Parser LispVal
parseRational = do n <- many1 digit
                   char '/'
                   d <- many1 digit
                   return $ Rational (read n, read d)

parseBool :: Parser LispVal
parseBool = do char '#'
               x <- oneOf "tf"
               return $ case x of 
                  't' -> Bool True
                  'f' -> Bool False

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many $ letter <|> digit <|> symbol
               let atom = [first] ++ rest
               return $ Atom atom

parseExpr :: Parser LispVal
parseExpr = parseChar
        <|> parseString
        <|> try parseFloat
        <|> try parseRational
        <|> parseNumber
        <|> parseBool
        <|> parseAtom
