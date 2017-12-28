{-# LANGUAGE TemplateHaskell #-}
module Data.Gerber.Interpreter where

import Data.Gerber.Types
import Control.Lens hiding (element)

data InterpreterState = InterpreterState
  { _commandsParsed :: Integer
  , _currentTool :: Integer}
  deriving (Show, Eq)

makeLenses ''InterpreterState

initState = InterpreterState
  { _commandsParsed = 0
  , _currentTool = 0}

eval :: Command -> InterpreterState -> InterpreterState
eval (ToolChange x) = over currentTool (const x)
eval _ = over commandsParsed (+1)
--  InterpreterState { commandsParsed = commandsParsed state + 1 }
--eval command state = -- over (commandsParsed) (+1)

evalBulk :: [Command] -> InterpreterState
evalBulk cmds = go cmds initState
  where
    go :: [Command] -> InterpreterState -> InterpreterState
    go []     imm = imm
    go (x:[])    imm = eval x imm
    go (x:xs) imm = go xs $ eval x imm

