{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.Ix
import Data.Array

import Control.Lens
import Control.Monad.State.Strict

import UI.NCurses

type Number       = Int
type SudokuNumber = Either Number (Maybe Number)

type Game a = StateT GameState Curses a

data Position = Position Vertical Horizon
              deriving (Show, Ix, Eq, Ord)

data Horizon  = L | C | R deriving (Show, Ix, Eq, Enum, Ord)
data Vertical = T | M | B deriving (Show, Ix, Eq, Enum, Ord)

data Board t = Board {
               _bGrid :: Array Position t
             , _bPosition :: Position
             }
             deriving Show

data GameState = GameState {
                 _gBoard :: Board (Board SudokuNumber)
               } 
               deriving Show

data MoveKey = KUp | KDown | KLeft | KRight deriving Show

data Event = MoveKey MoveKey
           | InputNum Number
           | Escape
           | Return
           | Quit
           deriving Show

$(makeLenses ''Board)
$(makeLenses ''GameState)
