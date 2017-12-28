{-# LANGUAGE OverloadedStrings #-}
module Lib where

import qualified Data.ByteString.Char8 as BSC
import Data.Gerber.Types
import qualified Data.Gerber as G

someFunc :: IO ()
someFunc = putStrLn "someFunc"

exampleInput :: BSC.ByteString
exampleInput = "G75*%MOMM*%"

exampleInputLong :: IO BSC.ByteString
exampleInputLong = BSC.pack <$> readFile "example/scale.gbr"

exampleParsedGerber :: IO (Either String [Command])
exampleParsedGerber = G.parseGerber <$> exampleInputLong
