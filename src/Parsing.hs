{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module Parsing where

import Arithmetic
import Boolean
import Control.Monad
import Data.Char
import ListCalculations
import State
import Syntax
import Text.ParserCombinators.Parsec
import Type
import TypeCheck

charParser :: Parser Char
charParser = do
  char '\''
  c <- letter
  char '\''
  return c

boolParser :: Parser Bool
boolParser =
  do
    string "true" >> return True
    <|> (string "false" >> return False)

intParser :: Parser Int
intParser = read <$> many1 digit

listParser :: Parser t -> Parser [t]
listParser parser = do
  char '['
  spaces
  list <- sepBy1 parser spaces
  spaces
  char ']'
  return list

bracketsParser :: Parser t -> Parser t
bracketsParser parser = do
  char '('
  spaces
  p <- parser
  spaces
  char ')'
  return p

emptyListParser :: Parser UExpression
emptyListParser =
  do
    string "i[]" >> return (ULit ([] :: [Int]))
    <|> (string "c[]" >> return (ULit ([] :: [Char])))
    <|> (string "b[]" >> return (ULit ([] :: [Bool])))

varParser :: Parser UExpression
varParser = do
  head <- letter
  var <- many1 alphaNum
  return $ UVar (head : var)

litParser :: Parser UExpression
litParser = do
  (ULit <$> boolParser)
    <|> (ULit <$> intParser)
    <|> (ULit <$> charParser)
    <|> (ULit <$> listParser intParser)
    <|> (ULit <$> listParser charParser)
    <|> (ULit <$> listParser boolParser)
    <|> emptyListParser

ariParser :: Parser BinOpAri
ariParser = do
  (char '+' >> return Add)
    <|> (char '*' >> return Mul)
    <|> (char '-' >> return Sub)
    <|> (char '\\' >> return Div)
    <|> (char '^' >> return Power)
    <|> (string "mod" >> return Mod)

intCalcParser :: Parser UExpression
intCalcParser = do
  op <- ariParser
  spaces
  l <- expressionParser
  spaces
  UIntCalc op l <$> expressionParser

ariBoolParser :: Parser OpAriBool
ariBoolParser =
  do
    char '>' >> return G
    <|> (char '<' >> return S)
    <|> (string "<=" >> return Se)
    <|> (string ">=" >> return Ge)
    <|> (string "==" >> return E)
    <|> (string "!=" >> return NE)

boolCalcParser :: Parser UExpression
boolCalcParser = do
  op <- ariBoolParser
  spaces
  l <- expressionParser
  spaces
  UBoolCalc op l <$> expressionParser

binOpBoolParser :: Parser BinOpBool
binOpBoolParser =
  do
    string "&&" >> return And
    <|> (string "||" >> return Or)

binBoolParser :: Parser UExpression
binBoolParser = do
  op <- binOpBoolParser
  spaces
  l <- expressionParser
  spaces
  UBinBool op l <$> expressionParser

unOpBoolParser :: Parser UnOpBool
unOpBoolParser = do
  char '!' >> return N

uniBoolParser :: Parser UExpression
uniBoolParser = do
  op <- unOpBoolParser
  UUniBool op <$> expressionParser

rangeParser :: Parser UExpression
rangeParser = do
  char '['
  spaces
  begin <- expressionParser
  spaces
  string ".."
  spaces
  end <- expressionParser
  spaces
  char ']'
  return $ URange begin end

elemParser :: Parser UExpression
elemParser = do
  list <- expressionParser
  string "!!"
  UElem list <$> expressionParser

listOpParser :: Parser ListOp
listOpParser =
  do
    string "\\" >> return LDiv
    <|> (char 'U' >> return Union)
    <|> (char 'I' >> return Intersect)
    <|> (string "++" >> return Concat)

listCalcParser :: Parser UExpression
listCalcParser = do
  op <- listOpParser
  spaces
  l <- expressionParser
  spaces
  UListCalc op l <$> expressionParser

toListParser :: Parser UExpression
toListParser = UToList <$> listParser expressionParser

expressionParser :: Parser UExpression
expressionParser = do
  spaces
  try (bracketsParser intCalcParser)
    <|> try (bracketsParser expressionParser)
    <|> try (bracketsParser boolCalcParser)
    <|> try (bracketsParser binBoolParser)
    <|> try (bracketsParser uniBoolParser)
    <|> try rangeParser
    <|> try (bracketsParser elemParser)
    <|> try (bracketsParser listCalcParser)
    <|> try toListParser
    <|> try litParser
    <|> try varParser

parseTest = parse expressionParser "" "     ([ 5 5 5]) "
