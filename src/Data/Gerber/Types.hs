module Data.Gerber.Types where

import Data.ByteString (ByteString)
import Data.Scientific hiding (scientific)

data Unit = MM | IN
  deriving (Show, Eq)

data Command =
  -- STANDARD COMMANDS
  -- G04
  Comment ByteString |
  -- Dxx, xx >= 10
  ToolChange Integer |
  Operation Coord Action |
  AddAperture Integer ByteString [Scientific] |
  DefineAperture ByteString [ByteString] |
  EndOfFile |
  --Standard ByteString |
  -- EXTENDED COMMANDS
  -- FSLAX
  FormatStatement FormatSpecification |
  -- MO
  SetUnits Unit |
  SetQuadrantMode QuadrantMode |
  -- G01/G02/G03
  SetInterpolationMode InterpolationMode |
  -- G36/G37
  SetRegionBoundary RegionBoundary |
  Deprecated DeprecatedType |
  SetOffset Integer Integer | -- Deprecated
  UnknownExtended ByteString |
  UnknownStandard ByteString
    deriving (Show, Eq)

data FormatSpecification = FormatSpecification
  { xDecimals :: Integer
  , yDecimals :: Integer}
  deriving (Show, Eq)

data DeprecatedType =
  OFA
    deriving (Show, Eq)

data ObjectPolarity = Dark | Clear
  deriving (Show, Eq)

--data Aperture =
--  Circle Scientific |
--  Rectangle Scientific Scientific
--    deriving (Show, Eq)

data QuadrantMode = SingleQuadrant | MultiQuadrant
  deriving (Show, Eq)

data InterpolationMode = Linear | Clockwise | CounterClockwise
  deriving (Show, Eq)

data RegionBoundary = StartRegion | EndRegion
  deriving (Show, Eq)

data Action = Draw
            | Move
            | Flash
  deriving (Show, Eq)

-- For convenience, Coord has fixed type
type CoordType = Integer
data Coord = Coord (Maybe CoordType) (Maybe CoordType)
  deriving (Show, Eq)

--instance Functor Coord where
--  fmap f (Coord x y) = Coord (f x) (f y)



