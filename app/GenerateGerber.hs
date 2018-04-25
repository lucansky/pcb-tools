import           Options.Applicative
import           Data.Semigroup ((<>))
import           Control.Lens hiding (argument,element,(#))
import           Control.Lens.Cons ((<|))
import           Data.Map.Lens
import           Data.List (sortBy,minimumBy)
import           Text.Pretty.Simple (pPrint)
import           Data.Scientific hiding (scientific)
import qualified Data.ByteString.Char8 as BS

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

programCore :: GenerateGerberOpts -> IO ()
programCore options = do
  --contents <- BS.openFile $ inputFile options
  let count = (read $ elementsCount options) :: Int

  putStrLn $ "Generat " <> (show count) <> " do suboru " <> (outputFile options)
  return ()

main :: IO ()
main = programCore =<< execParser programOptions

