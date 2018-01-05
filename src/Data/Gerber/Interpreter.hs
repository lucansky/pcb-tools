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
import Text.Pretty.Simple (pPrint)

--import Diagrams.Prelude
--import Diagrams.Backend.Rasterific.CmdLine
--import Diagrams.TwoD.Vector

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC

import Data.Scientific (Scientific)

import qualified Data.Map as Map
import Data.Map (Map)
import Lib

data ApertureTemplate = ApertureTemplate
  { templateName :: ByteString
  , templateDefinition :: [ByteString]}
  deriving (Show, Eq)

type ApertureTemplates = Map.Map ByteString ApertureTemplate

data Aperture = Aperture ByteString [Scientific]
  deriving (Show, Eq)
type Apertures = Map.Map Integer Aperture

data InterpreterState = InterpreterState
  { _formatSpecification :: Maybe FormatSpecification
  , _coordinateUnit :: Maybe Unit
  , _currentCoord :: Coord
  , _currentTool :: Integer
  , _interpolationMode :: Maybe InterpolationMode
  , _quadrantMode :: Maybe QuadrantMode
  -- TODO: polarity
  -- TODO: LM, LR, LS
  , _apertures :: Apertures
  , _apertureTemplates :: ApertureTemplates
  , _draws :: [Int]

  , _commandsParsed :: Integer
  , _unknownCommands :: Integer
  , _deprecatedCommands :: Integer}
  deriving (Show, Eq)

makeLenses ''InterpreterState

initStateM = InterpreterState
  { _formatSpecification = Nothing
  , _coordinateUnit = Nothing
  , _currentCoord = Coord Nothing Nothing
  , _currentTool = 0
  , _interpolationMode = Nothing
  , _quadrantMode = Nothing
  , _apertures = Map.empty
  , _apertureTemplates = Map.empty
  , _draws = []

  , _commandsParsed = 0
  , _unknownCommands = 0
  , _deprecatedCommands = 0}
    --where q = fromVertices [ 0 & 0, 0 & 2, 0.5 & 0.5, 2 & 0 :: Double ] # closePath

evalM :: Command -> State InterpreterState String
evalM (Comment comment) = do
  return $ "Comment " ++ BSC.unpack comment
evalM (ToolChange b) = do
  modify $ over (currentTool) (const b)
  return "Parsed one cmd."
evalM all@(Operation newCoord action) = do
  currentState <- get
  case action of
    Move -> return ()
    Draw -> do
      return ()
    Flash -> return ()

  modify $ currentCoord .~ newCoord
  return $ show all
evalM (AddAperture num name params) = do
  modify $ apertures . at num ?~ Aperture name params
  return $ "Added aperture"++BSC.unpack name
evalM (DefineAperture name def) = do
  modify $ apertureTemplates . at name ?~ ApertureTemplate name def
  return $ "Added aperture "++show name++" to templates."
evalM EndOfFile = return "End of stream."
evalM (FormatStatement fs) = do
  modify $ formatSpecification .~ Just fs
  return $ "Set format specification to "++show fs
evalM (SetUnits u) = do
  modify $ coordinateUnit .~ Just u
  return $ "Set coordinate units to "++show u
evalM (SetQuadrantMode qm) = do
  modify $ quadrantMode .~ Just qm
  return $ "Set quadrant mode to "++show qm
evalM (SetInterpolationMode im) = do
  modify $ interpolationMode .~ Just im
  return $ "Set interpolation mode to "++show im
evalM x         = return $ "Some command."

sampleList = [
    ToolChange 10,
    ToolChange 11,
    EndOfFile
  ]

try = do
  --(Right sampleList) <- exampleParsedGerber

  -- runState can be replaced with execState
  pPrint $ runState (mapM evalM (take 100 shortSnippet)) initStateM

