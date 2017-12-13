module Data.Gerber.Skel where

-- Haskell module generated by the BNF converter

import Data.Gerber.Abs
import Data.Gerber.ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transExp :: Exp -> Result
transExp x = case x of
  EAdd exp1 exp2 -> failure x
  EMul exp1 exp2 -> failure x
  EInt integer -> failure x
transCoord :: Coord -> Result
transCoord x = case x of
  ECoord exp -> failure x
