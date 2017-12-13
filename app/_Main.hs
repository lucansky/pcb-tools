{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import qualified System.IO as IO
import qualified System.Environment as E

import qualified Data.ByteString.Char8 as BS
import Data.Attoparsec.ByteString.Char8


uEol = takeWhile1 (/='\n')

data Unit = MM | IN
  deriving (Show)

data Action = Draw
            | Move
            | Flash
  deriving (Show)

data GerberLine = Tool
                | Line Action (Maybe Double) (Maybe Double)
                | Arc Action  (Maybe Double) (Maybe Double) Double Double
                | Percent BS.ByteString
                | Comment BS.ByteString
                | MO Unit
                | Other BS.ByteString
  deriving (Show)

parseComment :: Parser GerberLine
parseComment = Comment <$> ("G04" *> uEol)

parseAction = (pure Draw <$> "D01")
          <|> (pure Move <$> "D02")
          <|> (pure Flash <$> "D03")

parseLine :: Parser GerberLine
parseLine = do
          optional $ char 'X'
          x <- optional double
          optional $ char 'Y'
          y <- optional double
          a <- parseAction
          char '*'
          return $ Line a x y

parseArc :: Parser GerberLine
parseArc = do
          optional $ char 'X'
          x <- optional double
          optional $ char 'Y'
          y <- optional double
          char 'I'
          i <-  double
          char 'J'
          j <- double
          a <- parseAction
          char '*'
          return $ Arc a x y i j

parseSomething = takeWhile1(/='%')

parsePercent :: Parser GerberLine
parsePercent = do
             char '%'
             s <- parseSomething
             uEol
             return $ Percent s

parseMO :: Parser GerberLine
parseMO = MO . cwt <$> ("%MO" *> takeWhile1 (/='*') <* uEol)
    where
        cwt "MM" = MM
        cwt "IN" = IN

parseOther :: Parser GerberLine
parseOther = Other <$> uEol

parseGerberLine :: Parser GerberLine
parseGerberLine = parseComment
              <|> parseLine
              <|> parseArc
              <|> parsePercent
              <|> parseMO
              <|> parseOther

---------------------------------------------------------------
data Gerber = Gerber [GerberLine]
  deriving (Show)

parseGerber :: Parser Gerber
parseGerber = do
    lines <- many1 $ parseGerberLine <* endOfLine
    return $ Gerber lines

parseOGerber :: BS.ByteString -> Either String Gerber
parseOGerber = parseOnly parseGerber

main = do
    let file = "exlampe/edge.grb"
    putStrLn file
    f <- BS.readFile file
    print $ parseOGerber f
