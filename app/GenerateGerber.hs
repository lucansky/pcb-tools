import           Options.Applicative
import           Data.Semigroup ((<>))
import           Control.Lens hiding (argument,element,(#))
import           Control.Lens.Cons ((<|))
import           Data.Map.Lens
import           Data.List (sortBy,minimumBy)
import           Text.Pretty.Simple (pPrint)
import           Data.Scientific hiding (scientific)
import qualified Data.ByteString.Char8 as BS
import           System.Random
import           GenerateGerberTemplates

data GenerateGerberOpts = GenerateGerberOpts
  { elementsCount :: String,
    outputFile :: FilePath }
  deriving (Show, Eq)

optionParser :: Parser GenerateGerberOpts
optionParser = GenerateGerberOpts
    <$> strOption   (long "count"   <> help "how many elements should gerger contain")
    <*> argument str   (metavar "FILE")

programOptions :: ParserInfo GenerateGerberOpts
programOptions = info (optionParser <**> helper)
  ( fullDesc
  <> progDesc "Generates arbitraty gerber file"
  <> header "generategerber - gerber generator" )

generateDrawing :: (Int, Int, Int) -> String
generateDrawing (drawingIndex, x, y) = do
  let drawing = drawings !! drawingIndex
  drawing x y

programCore :: GenerateGerberOpts -> IO ()
programCore options = do
  let count = (read $ elementsCount options) :: Int

  putStrLn $ "Generating " <> (show count) <> " draws to file " <> (outputFile options) <> "."

  g <- newStdGen
  let max = (length drawings) - 1
  let randomSequence = take count (randomRs (0, max) g)

  g <- newStdGen
  let randomXCoordinates = take count (randomRs (100000, 20000) g)

  g <- newStdGen
  let randomYCoordinates = take count (randomRs (100000, 20000) g)

  let randomParamaters = zip3 randomSequence randomXCoordinates randomYCoordinates

  let generatedDrawings = concat $ map generateDrawing randomParamaters
  let output = gerberHeader <> generatedDrawings <> gerberFooter

--   putStrLn $ output
  writeFile (outputFile options) output

  return ()

main :: IO ()
main = programCore =<< execParser programOptions

