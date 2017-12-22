{-# LANGUAGE OverloadedStrings #-}
module Data.Gerber.Parser where

import Control.Applicative

import qualified Prelude
import Prelude hiding (takeWhile)

import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as C
import Data.Attoparsec.Combinator (choice)

import Data.Char (digitToInt)

import Data.Scientific hiding (scientific)

parseGerber input = do
  return $ parseOnly parseManyD input

data Unit = MM | IN
  deriving (Show, Eq)

--data Command = Standard ByteString | Extended ByteString
data Command =
  -- STANDARD COMMANDS
  -- G04
  Comment ByteString |
  -- Dxx, xx >= 10
  ToolChange Integer |
  Line Action Coord |
  AddAperture Integer ByteString [Scientific] |
  DefineAperture ByteString [ByteString] |
  EndOfFile |
  --Standard ByteString |
  -- EXTENDED COMMANDS
  -- FSLAX
  FormatSpecification Integer Integer Integer Integer |
  -- MO
  SetUnits Unit |
  SetQuadrantMode QuadrantMode |
  -- G01/G02/G03
  SetInterpolationMode InterpolationMode |
  -- G36/G37
  SetRegionBoundary RegionBoundary |
  Deprecated DeprecatedType |
  SetOffset Integer Integer | -- Deprecated
  UnknownExtended ByteString |
  UnknownStandard ByteString
    deriving (Show, Eq)

data DeprecatedType =
  OFA
    deriving (Show, Eq)

data ObjectPolarity = Dark | Clear
  deriving (Show, Eq)

data Aperture =
  Circle Scientific |
  Rectangle Scientific Scientific
    deriving (Show, Eq)

data QuadrantMode = SingleQuadrant | MultiQuadrant
  deriving (Show, Eq)

parseQuadrantMode :: Parser QuadrantMode
parseQuadrantMode = (pure SingleQuadrant <$> "G74")
                <|> (pure MultiQuadrant <$> "G75")

data InterpolationMode = Linear | Clockwise | CounterClockwise
  deriving (Show, Eq)

parseInterpolationMode = (pure Linear <$> "G01")
                     <|> (pure Clockwise <$> "G02")
                     <|> (pure CounterClockwise <$> "G03")

data RegionBoundary = StartRegion | EndRegion
  deriving (Show, Eq)

parseRegionBoundary = (pure StartRegion <$> "G36") <|> (pure EndRegion <$> "G37")

data Action = Draw
            | Move
            | Flash
  deriving (Show, Eq)

parseAction :: Parser Action
parseAction = (pure Draw <$> "D01")
          <|> (pure Move <$> "D02")
          <|> (pure Flash <$> "D03")

data Coord = Coord (Maybe Integer) (Maybe Integer)
  deriving (Show, Eq)

-- Low-level parsers
optionalNewLines = many (char '\n' <|> char '\r')
restOfCommand = takeWhile (/='*')

coords = Coord <$> maybeOption (char 'X' *> num) <*> maybeOption (char 'Y' *> num)

parseComment :: Parser Command
parseComment = Comment <$> ("G04" *> (optional $ char ' ') *> restOfCommand)

parseToolChange :: Parser Command
parseToolChange = ToolChange <$> (char 'D' *> num)

parseDCode :: Parser Command
parseDCode = do
  a <- coords
  x <- parseAction
  return $ Line x a

parseEOF :: Parser Command
parseEOF = (pure EndOfFile <$> "M02")

parseUnknownStandard :: Parser Command
parseUnknownStandard = do --Unknown <$> (many1 anyChar)
  x <- takeWhile1 (/='*')
  return $ UnknownStandard x

parseCommand :: Parser Command
parseCommand = parseExtendedCommand <|> parseStandardCommand

parseStandardCommand :: Parser Command
parseStandardCommand = do
  --_ <- takeWhile (/='*')
  x <-   parseComment
     <|> parseToolChange
     <|> parseDCode
     <|> (SetQuadrantMode <$> parseQuadrantMode)
     <|> (SetInterpolationMode <$> parseInterpolationMode)
     <|> (SetRegionBoundary <$> parseRegionBoundary)
     <|> parseEOF
     <|> parseUnknownStandard
  char '*'
  return x

parseExtendedCommand :: Parser Command
parseExtendedCommand = do
  char '%'
  x <- choice [parseFormatSpecification,
               parseSetUnits,
               parseAddAperture,
               parseApertureMacro,
               parseUnknownExtended]
  optional $ char '*'
  optional endOfLine
  char '%'
  return x

---
-- TODO: Each extended command should at the end parse "*%", except for
-- some, which can be %...*...*...*%
parseUnknownExtended :: Parser Command
parseUnknownExtended = UnknownExtended <$> (takeWhile1 (/='*') <* char '*')-- (takeTill (`elem` ['*']))

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
  apertures <- (sepBy (takeWhile (/='*')) $ char '*')

  return $ DefineAperture name apertures
    --where
      --modifier = do
      --  takeWhile $ inClass "A-Za-z0-9,.$"

---
num = signed decimal

parseManyD :: Parser [Command]
parseManyD = many1 (parseCommand <* optionalNewLines)
