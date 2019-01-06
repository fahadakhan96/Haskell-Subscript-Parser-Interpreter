module Parser.Impl where

-- Put your parser implementation in this file (and, if appropriate,
-- in other files in the Parser/ subdirectory)

import SubsAst

import Control.Monad
import Data.Char
import Text.Parsec.Prim hiding (token)
import Text.Parsec.Char
import Text.Parsec.Error
import Text.Parsec.String
import Text.Parsec.Combinator

-- You can change this if you want, but must be an instance of Show and Eq
-- data ParseError = ParseError String
--                 deriving (Show, Eq)

-- Create a munch that consumes unconditionally when predicate is satisfied
munch :: (Char -> Bool) -> Parser String
munch p = do
            s <- many $ satisfy p
            notFollowedBy $ satisfy p
            return s

-- Strip and tokenize
token :: Parser a -> Parser a
token t = do
            s <- t
            _ <- stripToken ""
            return s

-- Strip whitespace from both ends
stripToken :: String -> Parser String
stripToken s = try (spaces >> string s <* spaces)

keywords :: [String]
keywords = ["undefined", "false", "true", "of", "if","for"]

parseKeyword :: String -> Parser ()
parseKeyword k = token $ do
                        _ <- string k
                        notFollowedBy alphaNum

parseIdent :: Parser String
parseIdent = do
                x <- letter
                xs <- many $ alphaNum <|> char '_'
                spaces
                -- check if Ident is a keyword
                if (x:xs) `elem` keywords
                    then fail $ (x:xs) ++ "is a keyword"
                    else return (x:xs)

-- try used so that the first digit is not lost in case not negative
parseNumber :: Parser Expr
parseNumber = (try parseNegNumber <|> parsePosNumber) <* spaces

parsePosNumber :: Parser Expr
parsePosNumber = do
                    x <- many1 digit
                    if length x < 9
                        then return $ Number (read x)
                        else fail "Integer is too big."

parseNegNumber :: Parser Expr
parseNegNumber = do
                    neg <- char '-'
                    x <- many1 digit
                    if length x < 9
                        then return $ Number (read (neg:x))
                        else fail "Integer is too small."

-- Function for parsing Strings
parseExprString :: Parser Expr
parseExprString = token $ do
                            _ <- char '\''
                            s <- parseExprString' ""
                            return (String s)

-- Helper for parsing Strings
parseExprString' :: String -> Parser String
parseExprString' s1 = do
                        s2 <- munch doesCharBelong
                        b <- anyToken
                        case b of
                            '\'' -> return $ s1 ++ s2
                            -- checks for backslash in String
                            '\\' -> do
                                        s3 <- backslashCallback
                                        parseExprString' $ s1 ++ s2 ++ s3
                            _ -> fail "Unrecognized character found"

-- Function to skipWhitespace in between terms & expressions
skipWhitespace :: Parser a -> Parser a
skipWhitespace p = do
                s <- p
                spaces
                return s

-- Function to handle comments in file
handleComments :: Parser String
handleComments = try (do
                        _ <- string "//"
                        manyTill anyChar newline)
                      <|>
                      do
                        _ <- string "//"
                        manyTill anyChar eof -- ignore until EOF encountered

-- Function to parse whitespace
parseWhitespace :: Parser ()
parseWhitespace = do
                    a <- many $ oneOf " \n\t"
                    b <- many handleComments
                    when (not (null a) || not (null b)) $
                        do
                        parseWhitespace
                        return ()

-- Function to handle backslash in String
backslashCallback :: Parser String
backslashCallback = do
                        nextChar <- anyToken
                        case nextChar of
                            'n' -> return "\n"
                            '\'' -> return "'"
                            't' -> return "\t"
                            '\\' -> return "\\"
                            '\n' -> do
                                    parseWhitespace
                                    return ""
                            _ -> fail "Invalid backspace char."

-- Checks for Char invalid in String
doesCharBelong :: Char -> Bool
doesCharBelong '\'' = False
doesCharBelong '\\' = False
doesCharBelong c = isPrint c

parseTrue :: Parser Expr
parseTrue = skipWhitespace $ do
                                _ <- string "true"
                                notFollowedBy alphaNum
                                return TrueConst

parseFalse :: Parser Expr
parseFalse = skipWhitespace $ do
                                _ <- string "false"
                                notFollowedBy alphaNum
                                return FalseConst

parseUndefined :: Parser Expr
parseUndefined = skipWhitespace $ do
                                    _ <- string "undefined"
                                    notFollowedBy alphaNum
                                    return Undefined


parseParentheses :: Parser Expr
parseParentheses = do
                    void $ stripToken "("
                    expr <- parseExpr
                    void $ stripToken ")"
                    return expr

parseExpr :: Parser Expr
parseExpr = do
            expr <- parseExpr1
            parseExpr' expr

parseExpr' :: Expr -> Parser Expr
parseExpr' expr = do
                    void $ stripToken ","
                    Comma expr <$> parseExpr
                <|>
                    return expr

-- -- following is to implement operator precedence -- --
-- try used so expression is not lost incase of error
parseExpr1 :: Parser Expr
parseExpr1 = try (do
                    ident <- parseIdent
                    void $ stripToken "="
                    Assign ident <$> parseExpr1)
                <|>
                    parseExpr2

-- for parsing lt and eq
apAddOp :: Parser (Expr -> Expr -> Expr)
apAddOp = (do
            _ <- stripToken "<"
            return (\x y -> Call "<" [x, y]))
            <|>
            (do
                _ <- stripToken "==="
                return (\x y -> Call "===" [x, y]))

-- Chaining the equality operations
parseExpr2 :: Parser Expr
parseExpr2 = chainl1 parseExpr3 apAddOp

-- for parsing + and -
parseExpr3 :: Parser Expr
parseExpr3 = do
                expr <- parseExpr4
                parseExpr3' expr

parseExpr3' :: Expr -> Parser Expr
parseExpr3' expr = do
                    op <- stripToken "+" <|> stripToken "-"
                    expr' <- parseExpr4
                    parseExpr3' (Call op [expr, expr'])
                    <|>
                    return expr

-- for parsing * and %
parseExpr4 :: Parser Expr
parseExpr4 = do
                expr <- parseTerm
                parseWhitespace
                parseExpr4' expr

parseExpr4' :: Expr -> Parser Expr
parseExpr4' expr = do
                    op <- stripToken "*" <|> stripToken "%"
                    expr' <- parseTerm
                    parseExpr4' (Call op [expr, expr'])
                    <|>
                    return expr

-- recursively parsing multiple comma-separated expressions
parseExprs :: Parser [Expr]
parseExprs = do
                expr <- parseExpr1
                exps <- parseExprs'
                return (expr:exps)
            <|>
                return []

parseExprs' :: Parser [Expr]
parseExprs' = do
                void $ stripToken ","
                expr <- parseExpr1
                exps <- parseExprs'
                return (expr:exps)
            <|>
                return []

-- parsing arrays
parseArrayExpr :: Parser Expr
parseArrayExpr = do
                    void $ stripToken "["
                    parseArrayExpr'

parseArrayExpr' :: Parser Expr
parseArrayExpr' = try (do
                        exps <- parseExprs
                        void $ stripToken "]"
                        return (Array exps))
                    <|>
                        do
                            comprFor <- parseComprFor
                            void $ stripToken "]"
                            return (Compr comprFor)

parseTerm :: Parser Expr
parseTerm = parseNumber <|>
            parseExprString <|>
            try parseTrue <|>
            try parseFalse <|>
            try parseUndefined <|>
            parseParentheses <|>
            parseIdentExpr <|>
            parseArrayExpr

-- parse identity with expression
parseIdentExpr :: Parser Expr
parseIdentExpr = do
                    ident <- parseIdent
                    parseIdentExpr' ident

parseIdentExpr' :: Ident -> Parser Expr
parseIdentExpr' ident = do
                    void $ stripToken "("
                    exps <- parseExprs
                    void $ stripToken ")"
                    return (Call ident exps)   -- treat as a function
                    <|>
                    return (Var ident)     -- treat as a variable

-- parse for in comprehension
parseComprFor :: Parser ArrayCompr
parseComprFor = do
                parseKeyword "for"
                parseWhitespace
                _ <- stripToken "("
                ident <- parseIdent
                parseWhitespace
                parseKeyword "of"
                parseWhitespace
                expr <- parseExpr1
                _ <- stripToken ")"
                parseWhitespace
                ACFor ident expr <$> parseCompr

-- parse if conditionals in comprehension
parseComprIf :: Parser ArrayCompr
parseComprIf = do
                parseKeyword "if"
                _ <- stripToken "("
                expr <- parseExpr1
                _ <- stripToken ")"
                ACIf expr <$> parseCompr

-- parse Array Comprehension
parseCompr :: Parser ArrayCompr
parseCompr =    try parseComprFor
            <|>
                try parseComprIf
            <|>
                do ACBody <$> parseExpr1

parseString :: String -> Either ParseError Expr
parseString = parse parseUntilEOF "Parsing Error"

parseUntilEOF :: Parser Expr
parseUntilEOF = do
                parseWhitespace
                expr <- parseExpr
                eof
                return expr