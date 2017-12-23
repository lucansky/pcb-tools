{-# LANGUAGE OverloadedStrings #-}
module Data.Gerber.Parser where

import Control.Applicative

import qualified Prelude
import Prelude hiding (takeWhile)

import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BSC
import Data.Attoparsec.Combinator (choice)
import Data.Char (digitToInt)
import Data.Scientific hiding (scientific)
import Data.Gerber.Types

parseGerber input = parseOnly parseManyD input

parseQuadrantMode :: Parser QuadrantMode
parseQuadrantMode = SingleQuadrant <$ "G74"
                <|> MultiQuadrant <$ "G75"

parseInterpolationMode = Linear <$ "G01"
                     <|> Clockwise <$ "G02"
                     <|> CounterClockwise <$ "G03"

parseRegionBoundary = StartRegion <$ "G36"
                  <|> EndRegion <$ "G37"

parseAction :: Parser Action
parseAction = Draw <$ "D01"
          <|> Move <$ "D02"
          <|> Flash <$ "D03"

-- Low-level parsers
optionalNewLines = many (char '\n' <|> char '\r')
restOfCommand = takeWhile (/='*')

coords = Coord <$> maybeOption (char 'X' *> num) <*> maybeOption (char 'Y' *> num)

parseComment :: Parser Command
parseComment = Comment <$> ("G04" *> (char ' ') *> restOfCommand)

parseToolChange :: Parser Command
parseToolChange = ToolChange <$> (char 'D' *> num)

parseDCode :: Parser Command
parseDCode = do
  a <- coords
  x <- parseAction
  return $ Line x a

parseEOF :: Parser Command
parseEOF = pure EndOfFile <$> "M02"

parseUnknownStandard :: Parser Command
parseUnknownStandard = UnknownStandard <$> (takeWhile (const True))

parseCommand :: Parser Command
parseCommand = parseExtendedCommand <|> parseStandardCommand

parseStandardCommand :: Parser Command
parseStandardCommand =
  choice [parseComment,
            parseToolChange,
            parseDCode,
            SetQuadrantMode <$> parseQuadrantMode,
            SetInterpolationMode <$> parseInterpolationMode,
            SetRegionBoundary <$> parseRegionBoundary,
            EndOfFile <$ "M02",
            parseUnknownStandard]

parseExtendedCommand :: Parser Command
parseExtendedCommand = do
  choice [parseFormatSpecification,
            parseSetUnits,
            parseAddAperture,
            parseApertureMacro,
            parseUnknownExtended]

---
parseUnknownExtended :: Parser Command
parseUnknownExtended = UnknownExtended <$> (takeWhile1 (/='*'))

-- Make a parser optional, return Nothing if there is no match
-- Stolen from https://stackoverflow.com/questions/34142495/attoparsec-optional-parser-with-maybe-result
maybeOption :: Parser a -> Parser (Maybe a)
maybeOption p = option Nothing (Just <$> p)

parseFormatSpecification :: Parser Command
parseFormatSpecification = do
  "FSLA"
  char 'X'
  xN <- digit
  xM <- digit
  char 'Y'
  yN <- digit
  yM <- digit
  return $ FormatSpecification (z xN) (z xM) (z xN) (z xM)
  where z = (toInteger.digitToInt)

parseSetUnits :: Parser Command
parseSetUnits = SetUnits <$> (string "MO" *> ((pure MM <$> string "MM") <|> (pure IN <$> "IN")))

parseAddAperture :: Parser Command
parseAddAperture = AddAperture <$> ("ADD" *> decimal)
                               <*> (takeWhile $ inClass "A-Z0-9") <* (char ',')
                               <*> (sepBy scientific $ char 'X')

parseApertureMacro :: Parser Command
parseApertureMacro = do
  string "AM"
  name <- takeWhile1 (/='*')
  char '*'
  apertures <- sepBy (takeWhile (/='*')) $ char '*'

  return $ DefineAperture name apertures
    --where
      --modifier = do
      --  takeWhile $ inClass "A-Za-z0-9,.$"

--
nl = optionalNewLines

data GerberStatement = Standard ByteString | Extended [ByteString]
  deriving (Show, Eq)

parseStmtsGerber :: Parser [GerberStatement]
parseStmtsGerber = do
  many1 $ (e <|> eof <|> s) <* optionalNewLines
    where
      e   = Extended <$> (char '%' *> many1 (eat <* char '*' <* nl) <* char '%')
      s   = Standard <$> takeWhile1 (inClass allowedChars) <* char '*' <* nl
      eof = Standard <$> string "M02" <* char '*' <* many anyChar

      eat = takeWhile1 $ inClass allowedChars

allowedChars :: String
allowedChars = "A-Za-z0-9,.#@$\n"

---
num = signed decimal

parseManyD :: Parser [Command]
parseManyD = many1 (parseCommand <* optionalNewLines)
