--{-# LANGUAGE FlexibleInstances         #-}
--{-# LANGUAGE FlexibleContexts          #-}
--{-# LANGUAGE NoMonomorphismRestriction #-}
--{-# LANGUAGE TypeFamilies              #-}

import Options.Applicative
import Data.Semigroup ((<>))

--import Diagrams.Backend.Rasterific.CmdLine
--import Diagrams.Backend.CmdLine
--import Diagrams.Backend.SVG.CmdLine
import Diagrams.Backend.SVG
import Diagrams.TwoD.Size
import Diagrams.TwoD.Vector
import Diagrams.TwoD.Offset
import Diagrams.Prelude hiding (SizeSpec)

import Control.Lens.Cons ((<|))
import Text.Pretty.Simple (pPrint)
import Data.Scientific hiding (scientific)
import qualified Data.ByteString.Char8 as BS

import System.Remote.Monitoring
import Control.Concurrent (threadDelay)

import Control.Parallel
import Control.Parallel.Strategies

import Control.Concurrent.Async.Pool
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.STM
import Control.Concurrent

import Data.Gerber.Parser (parseGerber)
import Data.Gerber.Interpreter -- (evalGerberCommands)

data DrawerOpts = DrawerOpts
  { inputFile :: FilePath
  , outputFile :: FilePath
  } deriving (Show, Eq)

optionParser :: Parser DrawerOpts
optionParser = DrawerOpts
    <$> argument str (metavar "INPUT_FILE")
    <*> argument str (metavar  "OUTPUT_FILE")

programOptions :: ParserInfo DrawerOpts
programOptions = info (optionParser <**> helper)
  ( fullDesc
  <> progDesc "Print a greeting for TARGET"
  <> header "hello - a test for optparse-applicative" )

programCore :: DrawerOpts -> IO ()
programCore options = do
  contents <- BS.readFile $ inputFile options
  -- forkServer (BS.pack "localhost") 8000

  let parsed = parseGerber contents
  -- pPrint parsed
  case parsed of
    Left why -> error $ "unable to parse, err: " ++ show why
    Right commands -> do
      let m = evalGerberCommands commands
          drawings = m ^. draws
      d <- drawGerberIO drawings
          --opts = DiagramOpts Nothing (Just 400) "out/out.pdf"
      --pPrint drawings
      --mainWith $! drawGerber $! drawings
      --mainRender (DiagramOpts Nothing (Just 400) "out/out.pdf") (drawGerber drawings)
      let dim = mkSizeSpec2D (Just 1000) Nothing
      renderSVG (outputFile options) dim d
  return ()


drawGerberIO :: [([Scientific], b0, Located (Trail V2 Double))] -> IO (Diagram B)
--drawGerberIO draws = mconcat $ par (widenTrace <$> draws) -- parallel cariant
--drawGerberIO draws = return $ mconcat $ fmap widenTrace draws -- serial variant
drawGerberIO draws = do --return $ mconcat $ fmap widenTrace draws -- serial variant
  pool' <- createPool
  p <- createTaskGroup pool' 1000000
  putStrLn $ show (length draws)
  h <- atomically $ mapReduce p $ map (return . widenTrace) draws
  Async.withAsync (runTaskGroup p) $ const $ do
    x <- wait h
    return x
  where
    par = flip (using) $ (parListChunk 10000 rseq)
    widenTrace = (\(a,b,c) -> c # (e (toRealFloat $ head a)) # stroke # lc blue # lw ultraThin)
    -- (lineWidth $ local $ 1.0 *(toRealFloat $ head a) ))
    e thickness = expandTrail' opts (254 * thickness)
    opts = with & expandJoin .~ LineJoinRound & expandCap .~ LineCapRound
    trails = fmap third draws
    first (a, b, c) = a
    third (a, b, c) = c

drawGerber :: [([Scientific], b0, Located (Trail V2 Double))] -> Diagram B
drawGerber draws = mconcat $ par (widenTrace <$> draws) -- parallel cariant
--drawGerber draws = mconcat $ fmap widenTrace draws -- serial variant
  where
    par = flip (using) $ (parListChunk 10000 rseq)
    widenTrace = (\(a,b,c) -> c # (e (toRealFloat $ head a)) # stroke # lc blue # lw ultraThin)
    -- (lineWidth $ local $ 1.0 *(toRealFloat $ head a) ))
    e thickness = expandTrail' opts (254 * thickness)
    opts = with & expandJoin .~ LineJoinRound & expandCap .~ LineCapRound
    trails = fmap third draws
    first (a, b, c) = a
    third (a, b, c) = c

main :: IO ()
main = programCore =<< execParser programOptions
--main = programCore $ DrawerOpts "generated.gbr"

