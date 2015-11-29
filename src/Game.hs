module Game where

import Data.Maybe
import qualified Data.Set as S
import Data.Array

import Control.Lens

import Types
import Utils

emptyBoard of'
  = Board {
    _bGrid     = listArray posRange (repeat of')
  , _bPosition = Position T L
  }

insertNum :: S.Set Number -> SudokuNumber -> S.Set Number
insertNum s (Left a)         = S.insert a s
insertNum s (Right (Just a)) = S.insert a s
insertNum s (Right Nothing)  = s

blockCleared :: Board SudokuNumber -> Bool
blockCleared board
  = S.size (foldl insertNum S.empty (board^.bGrid)) == 9

rangeCleared :: (Position, Position) -> Board (Board SudokuNumber) -> Bool
rangeCleared p board
  = S.size (foldl insertNum S.empty allElems) == 9 
  where
    positions = range p
    allElems  = let grids = values (board^.bGrid) positions
                in  concatMap (\grid -> values (grid^.bGrid) positions) grids
    
    values array = \is -> map (array!) is 

gameCleared :: Board (Board SudokuNumber) -> Bool
gameCleared b = 
  all blockCleared (b ^. bGrid) && all (flip rangeCleared b) allLines
  where
    allLines = [ (Position T L, Position T R)
               , (Position M L, Position M R)
               , (Position B L, Position B R)
               , (Position T L, Position B L)
               , (Position T C, Position B C)
               , (Position T R, Position B R)
               ]

