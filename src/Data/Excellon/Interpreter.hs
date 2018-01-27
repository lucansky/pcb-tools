{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Excellon.Interpreter where

import           Control.Monad.State
import           Control.Monad (ap, (>=>))
import           Control.Lens hiding (element,(#))
import           Control.Lens.Cons ((<|))
import           Text.Pretty.Simple (pPrint)

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC

import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Monoid ((<>))
import           PCBTools.Common
import           Data.Excellon.Types hiding (at)
import           Data.Excellon.Parser

type InterpreterState = DrillJob

initStateM = defaultDrillJob

evalM :: ExcellonCommand -> State InterpreterState ()
evalM (M code) = do
  case code of
    71 -> modify $ drillUnit .~ MM
    72 -> modify $ drillUnit .~ IN
    otherwise -> return ()
evalM (AddDrill tool dia) = do
  modify $ drillsDefinition . at tool ?~ Drill dia
evalM (SetDrill toolIdent) = do
  modify $ lastUsedDrill .~ toolIdent
evalM (DrillAt drillpos_x drillpos_y) = do
  currentState <- get
  let p = Point2 (scaleUnits drillpos_x, scaleUnits drillpos_y)
      currentDrillIdent :: Integer
      currentDrillIdent = currentState ^. lastUsedDrill
  modify $ over drillings (flip mappend $ [(currentDrillIdent `atPos` p)])
    where
      scaleUnits = (/1000.0) . fromIntegral
evalM x         = do
  return ()

-- execState can be replaced with runState, will accumulate intermediate results (in this case units)
evalExcellonCommands :: Traversable t => t ExcellonCommand -> InterpreterState
evalExcellonCommands commands = execState (mapM evalM commands) initStateM

tryExcellonInterpreter = do
  ParsedExcellon h b <- tryExcellonParser
  return $ evalExcellonCommands (h <> b)

tryExcellonInterpreter' = tryExcellonInterpreter >>= pPrint

