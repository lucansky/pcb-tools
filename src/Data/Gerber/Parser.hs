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

parseGerber :: ByteString -> Either String [Command]
parseGerber input = parseOnly (parseGerber' <* endOfInput) input

nl = optionalNewLines

parseGerber' :: Parser [Command]
parseGerber' = many1 $ (eof <|> e <|> s) <* optionalNewLines
    where
      e   = (char '%' *> parseExtendedCommand <* char '%') <* nl
      s   = parseStandardCommand <* char '*' <* nl
      eof = EndOfFile <$ (string "M02" <* char '*' <* takeWhile (const True))

eat x = takeWhile1 $ inClass x

allowedChars :: String
allowedChars = "A-Za-z0-9,.#@$\n"

parseStandardCommand :: Parser Command
parseStandardCommand =
  choice $ [parseComment,
            parseToolChange,
            parseDCode,
            SetQuadrantMode <$> parseQuadrantMode,
            SetInterpolationMode <$> parseInterpolationMode,
            SetRegionBoundary <$> parseRegionBoundary,
            --EndOfFile <$ (string "M02"),
            parseUnknownStandard]

parseExtendedCommand :: Parser Command
parseExtendedCommand =
  parseApertureMacro <|> choice singleBlockParsers
  where
   singleBlockParsers = fmap singleBlock [parseFormatSpecification,
                          parseSetUnits,
                          parseAddAperture,
                          parseUnknownExtended]

---
parseQuadrantMode :: Parser QuadrantMode
parseQuadrantMode = SingleQuadrant <$ "G74"
                <|> MultiQuadrant <$ "G75"

parseInterpolationMode :: Parser InterpolationMode
parseInterpolationMode =
  Linear           <$ "G01" <|>
  Clockwise        <$ "G02" <|>
  CounterClockwise <$ "G03"

parseRegionBoundary = StartRegion <$ "G36"
                  <|> EndRegion <$ "G37"

-- Low-level parsers
optionalNewLines = many (char '\n' <|> char '\r')
restOfCommand = takeWhile (/='*')

parseComment :: Parser Command
parseComment = Comment <$> ("G04" *> (char ' ') *> restOfCommand)

parseToolChange :: Parser Command
parseToolChange = ToolChange <$> (char 'D' *> num)

parseAction :: Parser Action
parseAction = Draw <$ "D01"
          <|> Move <$ "D02"
          <|> Flash <$ "D03"

coords = Coord <$> maybeOption (char 'X' *> num) <*> maybeOption (char 'Y' *> num)

parseDCode :: Parser Command
parseDCode = Operation <$> coords <*> parseAction

parseEOF :: Parser Command
parseEOF = EndOfFile <$ "M02"

parseUnknownStandard :: Parser Command
parseUnknownStandard = UnknownStandard <$> eat allowedChars


-- Consumes single block extended command.
-- Basically drops '*' to make all subsequent parsers simpler.
singleBlock :: Parser a -> Parser a
singleBlock p = p <* char '*'

-- Last '*' in extended command is to be handled manually.
multiBlock p = p

---
parseUnknownExtended :: Parser Command
parseUnknownExtended = UnknownExtended <$> takeWhile1 (/='*')

-- Make a parser optional, return Nothing if there is no match
-- Stolen from https://stackoverflow.com/questions/34142495/attoparsec-optional-parser-with-maybe-result
maybeOption :: Parser a -> Parser (Maybe a)
maybeOption p = option Nothing (Just <$> p)

parseFormatSpecification :: Parser Command
parseFormatSpecification = do
  "FSLA"
  char 'X'
  _ <- digit
  xM <- digit
  char 'Y'
  _ <- digit
  yM <- digit
  return $ FormatStatement $ FormatSpecification (z xM) (z xM)
  where z = toInteger.digitToInt

parseSetUnits :: Parser Command
parseSetUnits = SetUnits <$> (string "MO" *> ((MM <$ "MM") <|> (IN <$ "IN")))

parseAddAperture :: Parser Command
parseAddAperture = AddAperture <$> ("ADD" *> decimal)
                               <*> takeWhile (inClass "A-Z0-9") <* char ','
                               <*> sepBy scientific (char 'X')

parseApertureMacro :: Parser Command
parseApertureMacro = do
  string "AM"
  name <- takeWhile1 (/='*')
  char '*' <* nl
  apertures <- sepBy1 (eat allowedChars) (char '*' <* optional nl)
  char '*' <* optional nl
  return $ DefineAperture name apertures

---
num = signed decimal
