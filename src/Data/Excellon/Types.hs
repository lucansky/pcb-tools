{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Excellon.Types where

import qualified Data.Map as Map
import           Data.Map (Map)
import           Control.Lens hiding (element,(#))
import           Control.Lens.Cons ((<|))

import           PCBTools.Common (Unit(..))

type Diameter = Double
data Drill = Drill { diameter :: Diameter }
  deriving (Show, Eq)

type CoordType = Double

-- Degree of freedom reduced to CoordType :(, FIXME with Linear.Vector
data Point2 = Point2 (CoordType, CoordType)
  deriving (Show, Eq)

data Located a = Loc { loc :: Point2, unLoc :: a}
  deriving (Show, Eq)

atPos :: a -> Point2 -> Located a
atPos what pos = Loc pos what

type ToolIdentifier = Integer

data DrillJob = DrillJob
  { _drillUnit        :: Unit
  , _drillsDefinition :: Map ToolIdentifier Drill
  , _drillings        :: [Located ToolIdentifier]
  , _lastUsedDrill    :: ToolIdentifier
  }
  deriving (Show, Eq)

defaultDrillJob :: DrillJob
defaultDrillJob = DrillJob MM mempty mempty 0

makeLenses ''DrillJob

