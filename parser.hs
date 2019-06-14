module Parser
  ( parseSpecification
  , pLanguage
  ) where
  
import DataStructures

-- parser mostly inspired by the inbound haskell parser
import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Either
import           Data.Set             (Set)
import qualified Data.Set             as S
import           System.IO

import qualified Text.Parsec          as P
import qualified Text.Parsec.Language as P
import qualified Text.Parsec.Token    as P

type Parser a = P.Parsec String () a




parseSpecification :: String -> Either P.ParseError Expr
parseSpecification = P.parse (pLanguage) ""




myDef = P.emptyDef 

brainfuckTokenParser = P.makeTokenParser myDef

pIdentifier = P.identifier brainfuckTokenParser

pBrackets = P.brackets brainfuckTokenParser

pSymbol = P.symbol brainfuckTokenParser

pColon = P.colon brainfuckTokenParser

pDot = P.dot brainfuckTokenParser

pComma = P.comma brainfuckTokenParser

pReserved = P.reserved brainfuckTokenParser

pParens = P.parens brainfuckTokenParser

pBraces = P.braces brainfuckTokenParser

pWhiteSpace = P.whiteSpace brainfuckTokenParser

pCommaSep1 = P.commaSep1 brainfuckTokenParser


parseIncrementP :: Parser Expr
parseIncrementP = do 
    pSymbol ">"
    a<-pStep
    return (IncrementP a)

parseDecrementP :: Parser Expr
parseDecrementP = do 
    pSymbol "<"
    a<-pStep
    return (DecrementP a )
parseDecrementV :: Parser Expr
parseDecrementV = do 
    pSymbol "-"
    a<-pStep
    return (DecrementV  a)
parseIncrementV :: Parser Expr
parseIncrementV = do 
    pSymbol "+"
    a<-pStep
    return (IncrementV a)


parseOutput :: Parser Expr
parseOutput =  do 
    pDot
    a<-pStep
    return (OutputV a)

parseInput :: Parser Expr
parseInput =  do 
    pComma
    c<-pInt 0
    a<-pStep
    return (InputV c a )

parseBrackets :: Parser Expr
parseBrackets = pBrackets $ do 
        a<-pStep 
        return a 
pFullBrackets :: Parser Expr 
pFullBrackets = do 
        c<-parseBrackets
        a<-pStep
        return (BracketCommand c a )
pStringInt :: Int->Parser Int
pStringInt str = do 
    a<-P.oneOf "0123456789"
    rest<-pInt ((str*10)+(read [a] ))
    return rest

pNotStringInt :: Int ->Parser Int
pNotStringInt str = (do 
    P.lookAhead (P.noneOf "0123456789")
    return str) <|> (do 
        P.eof
        return str)

pInt :: Int->Parser Int
pInt int = pStringInt int <|> pNotStringInt int 

pEOF :: Parser Expr
pEOF = do 
        P.eof
        return EndCommand
pEmpty :: Parser Expr
pEmpty = do 
        P.string ""
        return EndCommand

pStep :: Parser Expr
pStep = pFullBrackets <|> parseInput <|> parseOutput <|> parseIncrementV <|>  parseDecrementV <|> parseIncrementP <|> parseDecrementP <|> pEOF <|> pEmpty 


pLanguage :: Parser Expr
pLanguage = do 
    pWhiteSpace
    a<-pStep
    return a 

