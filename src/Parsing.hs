module Parsing where

import Arithmetic
import Boolean
import Control.Monad
import Data.Char
import Data.Ratio
import Language.Kuifje.Distribution hiding (return)
import ListCalculations
import State
import Syntax
import Text.Parsec.Char
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

integerParser :: Parser Integer
integerParser = read <$> many digit

listParser :: Parser t -> Parser [t]
listParser parser = do
  char '['
  spaces
  list <- sepBy parser (char ',')
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

stringParser :: Parser String
stringParser = do
  char '"'
  s <- many alphaNum
  char '"'
  return s

emptyListParser :: Parser UExpression
emptyListParser =
  do
    string "i[]" >> return (ULit ([] :: [Int]))
    <|> (string "c[]" >> return (ULit ([] :: [Char])))
    <|> (string "b[]" >> return (ULit ([] :: [Bool])))

idParser :: Parser String
idParser = do
  head <- letter
  var <- many alphaNum
  return $ head : var

varParser :: Parser UExpression
varParser = do
  UVar <$> idParser

litParser :: Parser UExpression
litParser = do
  (ULit <$> boolParser)
    <|> (ULit <$> intParser)
    <|> (ULit <$> charParser)
    <|> (ULit <$> listParser intParser)
    <|> (ULit <$> listParser charParser)
    <|> (ULit <$> stringParser)
    <|> (ULit <$> listParser boolParser)
    <|> emptyListParser

ariParser :: Parser BinOpAri
ariParser = do
  (char '+' >> return Add)
    <|> (char '*' >> return Mul)
    <|> (char '-' >> return Sub)
    <|> (char '/' >> return Div)
    <|> (char '^' >> return Power)
    <|> (string "mod" >> return Mod)

intCalcParser :: Parser UExpression
intCalcParser = do
  op <- ariParser
  many1 space
  l <- expressionParser
  many1 space
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
  many1 space
  l <- expressionParser
  many1 space
  UBoolCalc op l <$> expressionParser

binOpBoolParser :: Parser BinOpBool
binOpBoolParser =
  do
    string "&&" >> return And
    <|> (string "||" >> return Or)

binBoolParser :: Parser UExpression
binBoolParser = do
  op <- binOpBoolParser
  many1 space
  l <- expressionParser
  many1 space
  UBinBool op l <$> expressionParser

unOpBoolParser :: Parser UnOpBool
unOpBoolParser = do
  char '!' >> return N

uniBoolParser :: Parser UExpression
uniBoolParser = do
  op <- unOpBoolParser
  many1 space
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
  string "!!"
  list <- expressionParser
  UElem list <$> expressionParser

listOpParser :: Parser ListOp
listOpParser =
  do
    char 'D' >> return LDiv
    <|> (char 'U' >> return Union)
    <|> (char 'I' >> return Intersect)
    <|> (string "++" >> return Concat)

listCalcParser :: Parser UExpression
listCalcParser = do
  op <- listOpParser
  many1 space
  l <- expressionParser
  many1 space
  UListCalc op l <$> expressionParser

toListParser :: Parser UExpression
toListParser = UToList <$> listParser expressionParser

expressionParser :: Parser UExpression
expressionParser = do
  spaces
  try (bracketsParser intCalcParser)
    <|> try (bracketsParser boolCalcParser)
    <|> try (bracketsParser binBoolParser)
    <|> try (bracketsParser uniBoolParser)
    <|> try rangeParser
    <|> try (bracketsParser elemParser)
    <|> try (bracketsParser listCalcParser)
    <|> try toListParser
    <|> try litParser
    <|> try varParser

statementParser :: Parser UStatement
statementParser = do
  spaces
  var <- idParser
  spaces
  string "="
  spaces
  UAssign var <$> expressionParser

returnParser :: Parser String
returnParser = string "return"

uniformParser :: Parser String
uniformParser = string "uniform"

chooseParser :: Parser String
chooseParser = string "choose"

probParser :: Parser Prob
probParser = do
  t <- integerParser
  spaces
  char '%'
  spaces
  b <- integerParser
  return $ t % b

returnUpdateParser :: Parser UUpdateLanguage
returnUpdateParser = do
  returnParser
  many1 space
  s <- bracketsParser statementParser
  return $ UURet s

uniformUpdateParser :: Parser UUpdateLanguage
uniformUpdateParser =
  do
    uniformParser
    many1 space
    ( do
        s <- listParser statementParser
        return $ UUUni s
      )
    <|> ( do
            char '['
            spaces
            var <- idParser
            spaces
            char '='
            spaces
            l <- idParser
            spaces
            char '|'
            spaces
            r <- idParser
            spaces
            string "<-"
            spaces
            e <- expressionParser
            spaces
            char ']'
            if l == r && l /= var
              then return $ UUUniAssign var e
              else error "malformed list comprehension"
        )

chooseUpdateParser :: Parser UUpdateLanguage
chooseUpdateParser = do
  chooseParser
  many1 space
  p <- probParser
  many1 space
  l <- statementParser
  many1 space
  UUChoose p l <$> statementParser

uniAssignUpdateParser :: Parser UUpdateLanguage
uniAssignUpdateParser = do
  uniformParser
  spaces
  char '['
  spaces
  var <- idParser
  spaces
  char '='
  spaces
  l <- idParser
  spaces
  char '|'
  spaces
  r <- idParser
  spaces
  string "<-"
  spaces
  e <- expressionParser
  spaces
  char ']'
  if l == r && l /= var
    then return $ UUUniAssign var e
    else error "malformed list comprehension"

updateParser :: Parser UUpdateLanguage
updateParser = do
  returnUpdateParser
    <|> (try uniformUpdateParser)
    <|> chooseUpdateParser
    <|> (try uniAssignUpdateParser)

returnConditionParser :: Parser UConditionLanguage
returnConditionParser = do
  returnParser
  many1 space
  UCRet <$> expressionParser

uniformConditionParser :: Parser UConditionLanguage
uniformConditionParser = do
  uniformParser
  many1 space
  UCUni <$> listParser expressionParser

chooseConditionParser :: Parser UConditionLanguage
chooseConditionParser = do
  chooseParser
  many1 space
  p <- probParser
  many1 space
  l <- expressionParser
  many1 space
  UCChoose p l <$> expressionParser

conditionParser :: Parser UConditionLanguage
conditionParser =
  do
    returnConditionParser
    <|> uniformConditionParser
    <|> chooseConditionParser

returnObservationParser :: Parser UObserveLanguage
returnObservationParser = do
  returnParser
  many1 space
  UORet <$> expressionParser

uniformObservationParser :: Parser UObserveLanguage
uniformObservationParser = do
  uniformParser
  many1 space
  UOUni <$> expressionParser

chooseObservationParser :: Parser UObserveLanguage
chooseObservationParser = do
  chooseParser
  many1 space
  p <- probParser
  many1 space
  l <- expressionParser
  many1 space
  UOChoose p l <$> expressionParser

observationParser :: Parser UObserveLanguage
observationParser =
  do
    returnObservationParser
    <|> uniformObservationParser
    <|> chooseObservationParser

curlyParser :: Parser t -> Parser t
curlyParser parser = do
  char '{'
  spaces
  p <- parser
  spaces
  char '}'
  return p

skipParser :: Parser UBobby
skipParser = do
  string "skip"
  return USkip

endParser :: Parser UBobby
endParser =
  do
    char ';' >> return USkip
    <|> (spaces >> bobbyParser)

curlyEndParser :: Parser UBobby
curlyEndParser =
  do
    try (many space >> eof >> return USkip)
    <|> try (spaces >> spaces >> bobbyParser)

updateStatementParser :: Parser UBobby
updateStatementParser = do
  string "update"
  spaces
  u <- bracketsParser updateParser
  spaces
  UUpdate u <$> endParser

ifParser :: Parser UBobby
ifParser =
  do
    string "if"
    spaces
    b <- bracketsParser conditionParser
    spaces
    l <- curlyParser bobbyParser
    spaces
    ( ( do
          string "else"
          spaces
          r <- curlyParser bobbyParser
          spaces
          UIf b l r <$> curlyEndParser
      )
        <|> ( do
                char ';'
                spaces
                UIf b l USkip <$> bobbyParser
            )
      )

whileParser :: Parser UBobby
whileParser = do
  string "while"
  spaces
  b <- bracketsParser conditionParser
  spaces
  l <- curlyParser bobbyParser
  spaces
  UWhile b l <$> curlyEndParser

observeParser :: Parser UBobby
observeParser = do
  string "observe"
  many1 space
  e <- bracketsParser observationParser
  spaces
  UObserve e <$> endParser

bobbyParser :: Parser UBobby
bobbyParser =
  do
    try skipParser
    <|> try updateStatementParser
    <|> try ifParser
    <|> try whileParser
    <|> try observeParser
