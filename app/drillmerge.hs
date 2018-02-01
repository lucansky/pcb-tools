import           Options.Applicative
import           Data.Semigroup ((<>))
import           Control.Lens hiding (argument,element,(#))
import           Control.Lens.Cons ((<|))
import           Data.Map.Lens
import           Data.List (sortBy)
import           Text.Pretty.Simple (pPrint)
import           Data.Scientific hiding (scientific)
import qualified Data.ByteString.Char8 as BS

import           Data.Excellon.Types
import           Data.Excellon.Parser (parseExcellon, extractCommands)
import           Data.Excellon.Interpreter -- (evalGerberCommands)

data DrillMergeOpts = DrillMergeOpts
  { availableDrills :: String,
    inputFile :: FilePath }
  deriving (Show, Eq)

optionParser :: Parser DrillMergeOpts
optionParser = DrillMergeOpts
    <$> strOption   (long "drills"   <> help "available drill sizes")
    <*> argument str   (metavar "FILE")

programOptions :: ParserInfo DrillMergeOpts
programOptions = info (optionParser <**> helper)
  ( fullDesc
  <> progDesc "Merges diameters of the drills in Excellon (.drl/drd.) by rounding to nearest available drill tool."
  <> header "mergedrill - unify drill diameters" )

programCore :: DrillMergeOpts -> IO ()
programCore options = do
  contents <- BS.readFile $ inputFile options
  let (Right parsed) = parseExcellon contents
      drillJob = (evalExcellonCommands . extractCommands) parsed
      availableDrills = [0.8,0.9,1.0,3.0]

  putStrLn $ show parsed
  putStrLn $ show drillJob

  putStrLn $ show $ over drillsDefinition (fmap (\(Drill dia) -> Drill (findNearest availableDrills dia))) $ drillJob

  return ()

findNearest :: (Ord b, Num b) => [b] -> b -> b
findNearest available current = fst $ head $ sortBy (\(_,x)->(\(_,y)-> x `compare` y)) $ fmap (\x -> (x, abs (x-current))) available

main :: IO ()
main = programCore =<< execParser programOptions
--main = programCore $ DrillMergeOpts "example/scale.drd"

