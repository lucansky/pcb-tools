{-# LANGUAGE TemplateHaskell #-}
module Data.Gerber.Interpreter where

import Data.Gerber.Types
import Control.Monad.State
import Control.Monad (ap)
import Control.Lens hiding (element)
import Text.Pretty.Simple (pPrint)
import qualified Data.Map as Map
import Data.Map (Map)
import Lib

data Aperture = Aperture
  deriving (Show, Eq)
type Apertures = Map.Map Integer Aperture

data InterpreterState = InterpreterState
  { _commandsParsed :: Integer
  , _currentTool :: Integer
  , _currentCoord :: Coord
  , _apertures :: Apertures}
  deriving (Show, Eq)

makeLenses ''InterpreterState

initStateM = InterpreterState
  { _commandsParsed = 0
  , _currentTool = 0
  , _currentCoord = Coord Nothing Nothing
  , _apertures = Map.empty}

evalM :: Command -> State InterpreterState String
evalM (ToolChange b) = do
  modify $ over (currentTool) (const b)
  modify $ over (commandsParsed) (+1)
  return "Parsed one cmd."
evalM (Operation coord action) = do
  case action of
    Move -> modify $ over currentCoord (const coord)
    otherwise -> return ()

  modify $ over (commandsParsed) (+1)
  return "Operation."

evalM EndOfFile = return "End of stream."
evalM x         = return $ "Some command." ++ show x

sample1 = ToolChange 10
sample2 = ToolChange 11

sampleList = [
  ToolChange 10,
  ToolChange 11,
  EndOfFile
             ]

try = do
  (Right sampleList) <- exampleParsedGerber

  -- runState can be replaced with execState
  print $ runState (mapM evalM sampleList) initStateM
