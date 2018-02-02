module Data.Excellon.Parser where

import qualified Prelude
import           Prelude hiding (takeWhile)

import           Control.Applicative
import           Data.Monoid ((<>))
import           Data.ByteString (ByteString)
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BSC
import           Data.Attoparsec.Combinator (choice)
import           Data.Char (digitToInt)
import           Data.Scientific hiding (scientific)

import           Text.Pretty.Simple (pPrint)

import           Data.Excellon.Types
import           PCBTools.Common

data ExcellonCommand =
    -- Mxx command located in Body section
    M Integer |

    -- TxxCyy command in header (x - Tool identifier, y - diameter)
    AddDrill ToolIdentifier Diameter |

    -- Txx
    -- Sets current drill (T01, T02, T3)
    -- T0 means no drill, usually at the end of program
    SetDrill Integer |

    -- Marks the drill position
    DrillAt {x :: Integer, y :: Integer}
  deriving (Show, Eq)

data ParsedExcellon = ParsedExcellon
  { headerCommands :: [ExcellonCommand]
  , bodyCommands   :: [ExcellonCommand]}
  deriving (Show, Eq)

extractCommands :: ParsedExcellon -> [ExcellonCommand]
extractCommands p = headerCommands p <> bodyCommands p

parseExcellon :: ByteString -> Either String ParsedExcellon
parseExcellon input = parseOnly (parseExcellon' <* endOfInput) input

-- > <S> ::= <excellonCommands>
-- > <excellonCommands> ::= <header> <body>
parseExcellon' :: Parser ParsedExcellon
parseExcellon' = (ParsedExcellon <$> header <*> body) <* optionalNewLines
  where
      -- > <header> ::= "%" {<headerCommand> <newlines>} "%"
      header :: Parser [ExcellonCommand]
      header = (nl *> char '%' *> nl) *> many1 (parseHeaderCommand <* nl) <* (char '%' *> nl)
      -- > <body> ::= {<bodyCommand> <newlines>}
      body   :: Parser [ExcellonCommand]
      body   = many (parseBodyCommand <* nl)

-- --------------------------------------------
-- GENERIC PARSERS
-- --------------------------------------------
-- > <commandM> ::= "M" integer
genericMCommand :: Parser ExcellonCommand
genericMCommand = M <$> (char 'M' *> decimal)

-- > <genericCommand> ::= <commandM>
-- Parsers for commands which may be located header or body.
-- Technically handles overlapping commands.
genericParsers :: [Parser ExcellonCommand]
genericParsers = [genericMCommand]

-- --------------------------------------------
-- HEADER COMMANDS
-- --------------------------------------------
-- > <headerCommand> ::= <genericCommand> | <addDrill>
parseHeaderCommand = choice $ genericParsers <> [parseAddDrillCommand]

-- > <addDrill> ::= "T" integer "C" scientific
parseAddDrillCommand = AddDrill <$> (char 'T' *> decimal) <*> (char 'C' *> rational)

-- --------------------------------------------
-- BODY COMMANDS
-- --------------------------------------------
-- > <bodyCommand> ::= <genericCommand> | <setDrill> | <drillAt>
parseBodyCommand = choice $ genericParsers <> [parseSetDrillCommand, parseDrillAtCommand]

-- > <setDrill> ::= "T" integer
parseSetDrillCommand = SetDrill <$> (char 'T' *> decimal)

-- > <drillAt> ::= "X" integer "Y" integer
parseDrillAtCommand = DrillAt <$> (char 'X' *> num) <*> (char 'Y' *> num)

-- --------------------------------------------
-- PARTIAL PARSERS
-- --------------------------------------------

eat x = takeWhile1 $ inClass x
nl = optionalNewLines

allowedChars :: String
allowedChars = "A-Za-z0-9,.#@$\n"

-- Low-level parsers
-- > <newLines> ::= {"\n" | "\r"}
optionalNewLines = many (char '\n' <|> char '\r')
restOfCommand = takeWhile (/='*')
num = signed decimal

-----------------------------------------------
tryExcellonParser :: IO ParsedExcellon
tryExcellonParser = do
  contents <- BSC.readFile $ "example/scale.drd"
  pPrint $ show contents

  let (Right parsed) = parseExcellon $ contents
  putStrLn $ show parsed

  return parsed

