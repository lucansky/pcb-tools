import           Options.Applicative
import           Data.Semigroup ((<>))
import           Control.Lens.Cons ((<|))
import           Text.Pretty.Simple (pPrint)
import           Data.Scientific hiding (scientific)
import qualified Data.ByteString.Char8 as BS

import           Data.Excellon.Parser (parseExcellon)
import           Data.Excellon.Interpreter -- (evalGerberCommands)

data DrillMergeOpts = DrillMergeOpts
  { inputFile :: FilePath
  } deriving (Show, Eq)

optionParser :: Parser DrillMergeOpts
optionParser = DrillMergeOpts
    <$> argument str   (metavar "FILE")

programOptions :: ParserInfo DrillMergeOpts
programOptions = info (optionParser <**> helper)
  ( fullDesc
  <> progDesc "Merges diameters of the drills in Excellon (.drl/drd.) by rounding to nearest available drill tool."
  <> header "mergedrill - unify drill diameters" )

programCore :: DrillMergeOpts -> IO ()
programCore options = do
  contents <- BS.readFile $ inputFile options
  let parsed = parseExcellon contents
  putStrLn $ show parsed
  return ()

main :: IO ()
main = programCore =<< execParser programOptions
--main = programCore $ DrillMergeOpts "example/scale.drd"

