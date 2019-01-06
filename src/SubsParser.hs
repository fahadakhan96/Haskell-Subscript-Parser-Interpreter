module SubsParser (
    ParseError,
    parseString,
    parseFile
  ) where

import Control.Applicative

import SubsAst
import Parser.Impl

import Text.Parsec.Error

-- shouldn't need to change this
parseFile :: FilePath -> IO (Either ParseError Expr)
parseFile path = parseString Control.Applicative.<$> readFile path
