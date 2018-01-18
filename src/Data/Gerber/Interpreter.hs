{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Gerber.Interpreter where

import Data.Gerber.Types
import Control.Monad.State
import Control.Monad (ap, (>=>))
import Control.Lens hiding (element,(#))
import Control.Lens.Cons ((<|))
import Text.Pretty.Simple (pPrint)

import Diagrams.Prelude hiding (at)
--import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.TwoD.Vector

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC

import Data.Scientific (Scientific)

import qualified Data.Map as Map
import Data.Map (Map)
import Lib

type ApertureID = Integer

data ApertureTemplate = ApertureTemplate
  { templateName :: ByteString
  , templateDefinition :: [ByteString]}
  deriving (Show, Eq)

type ApertureTemplates = Map.Map ByteString ApertureTemplate
type ApertureParams = [Scientific]

data Aperture = Aperture
  { apertureName :: ByteString
  , apertureParams :: ApertureParams}
  deriving (Show, Eq)
type Apertures = Map.Map ApertureID Aperture

data InterpreterState = InterpreterState
  { _formatSpecification :: Maybe FormatSpecification
  , _coordinateUnit :: Maybe Unit
  , _currentCoord :: Coord
  , _currentAperture :: ApertureID
  , _interpolationMode :: Maybe InterpolationMode
  , _quadrantMode :: Maybe QuadrantMode
  -- TODO: polarity
  -- TODO: LM, LR, LS
  , _apertures :: Apertures
  , _apertureTemplates :: ApertureTemplates

  -- Should generalize over TrailLike
  , _draws :: [(ApertureParams, ApertureTemplate, Located (Trail V2 Double))]

  , _flashes :: [(ApertureParams, ApertureTemplate, Coord)]

  , _commandsParsed :: Integer
  , _unknownCommands :: Integer
  , _deprecatedCommands :: Integer}
  deriving (Show, Eq)

makeLenses ''InterpreterState

initStateM = InterpreterState
  { _formatSpecification = Nothing
  , _coordinateUnit = Nothing
  , _currentCoord = Coord Nothing Nothing
  , _currentAperture = 0
  , _interpolationMode = Nothing
  , _quadrantMode = Nothing
  , _apertures = Map.empty
  , _apertureTemplates = Map.fromList [("C", ApertureTemplate "C" []), ("R", ApertureTemplate "R" [])]
  , _draws = mempty
  , _flashes = mempty

  , _commandsParsed = 0
  , _unknownCommands = 0
  , _deprecatedCommands = 0}
    --where q = fromVertices [ 0 & 0, 0 & 2, 0.5 & 0.5, 2 & 0 :: Double ] # closePath

x = (3 ^& 6) :: Point V2 CoordType
y = (4 ^& 6) :: Point V2 CoordType

convertCoord :: Coord -> P2 Double
convertCoord (Coord (Just x) (Just y)) = p2 (conv x, conv y)
  where conv = fromIntegral

constructLine :: Coord -> Coord -> Located (Trail V2 Double)
constructLine from to = convertCoord from ~~ convertCoord to

evalM :: Command -> State InterpreterState ()
evalM (Comment comment) = do
  --tell $ "Comment " ++ BSC.unpack comment
  return ()
evalM (ToolChange b) = do
  modify $ over (currentAperture) (const b)
  --tell "Parsed one cmd."
evalM all@(Operation newCoord action) = do
  currentState <- get
  let apertureID = currentState ^. currentAperture
      -- TODO: Hits bottom in case of non-existent index !!
      (Just aperture)   = currentState ^. apertures . at apertureID
      apertureTemplate :: ApertureTemplate
      (Just apertureTemplate) = currentState ^. apertureTemplates . at (apertureName aperture)
      oldCoord = currentState ^. currentCoord
  -- TODO: Polygon drawing missing G36/G37
  case action of
    Move -> return ()
    Draw -> do
      mtell draws $ (apertureParams aperture, apertureTemplate, constructLine oldCoord newCoord)
    Flash -> do
      mtell flashes (apertureParams aperture, apertureTemplate, newCoord)

  modify $ currentCoord .~ newCoord
    where
      mtell attr what = modify $ over attr ((:) what)
evalM (AddAperture num name params) = do
  modify $ apertures . at num ?~ Aperture name params
  --tell $ "Added aperture"++BSC.unpack name
evalM (DefineAperture name def) = do
  modify $ apertureTemplates . at name ?~ ApertureTemplate name def
  --tell $ "Added aperture "++show name++" to templates."
evalM EndOfFile = return ()
  --tell "End of stream."
evalM (FormatStatement fs) = do
  modify $ formatSpecification .~ Just fs
  --tell $ "Set format specification to "++show fs
evalM (SetUnits u) = do
  modify $ coordinateUnit .~ Just u
  --tell $ "Set coordinate units to "++show u
evalM (SetQuadrantMode qm) = do
  modify $ quadrantMode .~ Just qm
  --tell $ "Set quadrant mode to "++show qm
evalM (SetInterpolationMode im) = do
  modify $ interpolationMode .~ Just im
  --tell $ "Set interpolation mode to "++show im
evalM x         = do
  --tell $ "Some command."
  return ()

-- execState can be replaced with runState, will accumulate intermediate results (in this case units)
evalGerberCommands :: Traversable t => t Command -> InterpreterState
evalGerberCommands commands = execState (mapM evalM commands) initStateM

try = evalGerberCommands (take 400 shortSnippet)
tryP = pPrint try

