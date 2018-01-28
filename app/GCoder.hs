import Options.Applicative
import Data.Semigroup ((<>))

import Data.Scientific hiding (scientific)

data GCoderOpts = GCoderOpts
  { frontFile :: FilePath
  , backFile :: FilePath
  , outlineFile :: FilePath
  , drilFile :: FilePath
  , zWork :: Scientific
  } deriving (Show, Eq)

optionParser :: Parser GCoderOpts
optionParser = GCoderOpts
    <$> strOption   (long "front"   <> help "front side RS274-X .gbr")
    <*> strOption   (long "back"    <> help "back side RS274-X .gbr")
    <*> strOption   (long "outline" <> help "pcb outline polygon RS274-X .gbr")
    <*> strOption   (long "drill"   <> help "Excellon drill file")
    <*> option auto (long "zwork"   <> metavar "NUMBER" <> help "milling depth (Z-coordinate while engraving)")

programOptions :: ParserInfo GCoderOpts
programOptions = info (optionParser <**> helper)
  ( fullDesc
  <> progDesc "Print a greeting for TARGET"
  <> header "hello - a test for optparse-applicative" )

programCore :: GCoderOpts -> IO ()
programCore options = do
  -- TBA real usage
  putStrLn $ show options

  -- Flow:
  -- 1. parseGerber [front, back, outline]
  -- 2. parseExcellon [drill]
  -- 3. interpretGerber front   >>= polygonize offset count >>= exportGCode options
  -- 4. interpretGerber back    >>= polygonize offset count >>= exportGCode options
  -- 5. interpretGerber outline >>= polygonize offset count >>= exportGCode options
  -- 6. interpretExcellon       >>= TSP options >>= exportGCodeDrilling options

main :: IO ()
main = programCore =<< execParser programOptions

