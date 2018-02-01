{-# LANGUAGE OverloadedStrings #-}
module Data.Gerber.Parser where

import qualified Prelude
import           Prelude hiding (takeWhile)

import           Control.Applicative
import           Data.ByteString (ByteString)
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BSC
import           Data.Attoparsec.Combinator (choice)
import           Data.Char (digitToInt)
import           Data.Scientific hiding (scientific)

import           Data.Gerber.Types
import           PCBTools.Common

parseGerber :: ByteString -> Either String [Command]
parseGerber input = parseOnly (parseGerber' <* endOfInput) input

nl = optionalNewLines

-- > <S> ::= <gerberCommands>
-- > <gerberCommands> ::= {"%" <extended> "%" | <standard> "*" | <eof> }
-- > <char>   ::= any ASCII char
-- > <eof>    ::= "M02*" {<anyChar>}
parseGerber' :: Parser [Command]
parseGerber' = many1 $ (eof <|> extended <|> standard) <* optionalNewLines
    where
      extended = (char '%' *> parseExtendedCommand <* char '%') <* nl
      standard = parseStandardCommand <* char '*' <* nl
      eof      = EndOfFile <$ (string "M02" <* char '*' <* takeWhile (const True))

-- > <anyCharExceptAsterisk> ::= [ASCII] - ["*"]
-- > <allowedChars> ::= A-Za-z0-9,.#@$\n
allowedChars :: String
allowedChars = "A-Za-z0-9,.#@$\n"

-- Low-level parsers
eat x = takeWhile1 $ inClass x

-- > <optionalNewLines> ::= {"\n" | "\r"}
optionalNewLines = many (char '\n' <|> char '\r')

-- > <takeTillAsteriskMany> ::= (<anyCharExceptAsterisk>)* "*"
-- > <takeTillAsteriskMany1> ::= <anyCharExceptAsterisk> <takeTillAsteriskMany>
restOfCommand = takeWhile (/='*')

-- Consumes single block extended command.
-- Basically drops '*' to make all subsequent parsers simpler.
-- > <singleBlockWrap> ::= <singleBlockExtendedCommand> "*"
singleBlock :: Parser a -> Parser a
singleBlock p = p <* char '*'

-- Last '*' in extended command is to be handled manually.
-- > <multiBlockWrap> ::= <multiBlockExtendedCommand>
multiBlock p = p

--
-- > <standard> ::= <comment>
-- > <standard> ::= <toolChange>
-- > <standard> ::= <operation>
-- > <standard> ::= <quadrantMode>
-- > <standard> ::= <interpolationMode>
-- > <standard> ::= <regionBoundary>
-- > <standard> ::= <unknownStandard>
parseStandardCommand :: Parser Command
parseStandardCommand =
  choice $ [parseComment,
            parseToolChange,
            parseOperation,
            SetQuadrantMode <$> parseQuadrantMode,
            SetInterpolationMode <$> parseInterpolationMode,
            SetRegionBoundary <$> parseRegionBoundary,
            --EndOfFile <$ (string "M02"),
            parseUnknownStandard]

-- > <extended> ::= <singleBlockCommand> "*" | <multiBlockCommand>
-- > <singleBlockCommand> ::= <formatSpecification>
-- > <singleBlockCommand> ::= <setUnits>
-- > <singleBlockCommand> ::= <addAperture>
-- > <singleBlockCommand> ::= <unknownExtended>
-- > <multiBlockCommands> ::= <apertureMacro>
parseExtendedCommand :: Parser Command
parseExtendedCommand =
  parseApertureMacro <|> choice singleBlockParsers
  where
   singleBlockParsers = fmap singleBlock [parseFormatSpecification,
                          parseSetUnits,
                          parseAddAperture,
                          parseUnknownExtended]

---

-- > <quadrantMode> ::= "G74" | "G75"
parseQuadrantMode :: Parser QuadrantMode
parseQuadrantMode = SingleQuadrant <$ "G74"
                <|> MultiQuadrant <$ "G75"

-- > <interpolationMode> ::= "G01" | "G02" | "G03"
parseInterpolationMode :: Parser InterpolationMode
parseInterpolationMode =
  Linear           <$ "G01" <|>
  Clockwise        <$ "G02" <|>
  CounterClockwise <$ "G03"

-- > <regionBoundary> ::= "G36" | "G37"
parseRegionBoundary = StartRegion <$ "G36"
                  <|> EndRegion <$ "G37"


-- > <comment> ::= "G04 " <commentChars> "*"
-- > <commentChars> ::= [ASCII] - ["*"]
parseComment :: Parser Command
parseComment = Comment <$> ("G04" *> (char ' ') *> restOfCommand)

-- > <toolChange> ::= "D" integer {integer}
parseToolChange :: Parser Command
parseToolChange = ToolChange <$> (char 'D' *> num)

-- > <action> ::= "D01" | "D02" | "D03"
parseAction :: Parser Action
parseAction = Draw <$ "D01"
          <|> Move <$ "D02"
          <|> Flash <$ "D03"

-- > <coord> ::= ["X" integer] ["Y" integer] ["I" integer] ["J" integer]
coords = Coord <$> maybeOption (char 'X' *> num) <*> maybeOption (char 'Y' *> num) -- FIXME: I,J

-- > <operation> ::= <coord> <action>
parseOperation :: Parser Command
parseOperation = Operation <$> coords <*> parseAction

-- > <unknownStandard> ::= (<anyCharExceptAsterisk>)* "*"
parseUnknownStandard :: Parser Command
parseUnknownStandard = UnknownStandard <$> eat allowedChars

---
-- > <unknownExtended> ::= (<anyCharExceptAsterisk>)* "*"
parseUnknownExtended :: Parser Command
parseUnknownExtended = UnknownExtended <$> takeWhile1 (/='*')

-- Make a parser optional, return Nothing if there is no match
-- Stolen from https://stackoverflow.com/questions/34142495/attoparsec-optional-parser-with-maybe-result
maybeOption :: Parser a -> Parser (Maybe a)
maybeOption p = option Nothing (Just <$> p)

-- > <formtSpecification> ::= "FSLA" "X" digit digit "Y" digit digit
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

-- > <setUnits> ::= "MO" ("MM" | "IN")
parseSetUnits :: Parser Command
parseSetUnits = SetUnits <$> (string "MO" *> ((MM <$ "MM") <|> (IN <$ "IN")))

-- > <addAperture> ::= "ADD" integer ([A-Z0-9]+) "," ({scientific "X"} | scientific)
parseAddAperture :: Parser Command
parseAddAperture = AddAperture <$> ("ADD" *> decimal)
                               <*> takeWhile1 (inClass "A-Z0-9") <* char ','
                               <*> sepBy scientific (char 'X')

-- > <apertureMacro> ::= "AM" <allowedChars>* "*" <apertures>
-- > <apertures> ::= <singleAperture> {<singleAperture>}
-- > <singleAperture> ::= <allowedChars>* "*" <optionalNewLines>
-- FIXME: parses, but does not give a meaning to the data
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
