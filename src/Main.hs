{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Main where

import Data.Array ((!))
import Data.Maybe
import Data.String.Utils

import Control.Monad 
import Control.Monad.State.Strict

import UI.NCurses
import System.Directory

import Lens.Simple

import Types
import Game
import Draw
import Utils

main :: IO ()
main = do
  levelFiles <- liftM (filter (endswith ".level")) $ getDirectoryContents "levels/" 
  when (null levelFiles) $ fail "no level found"

  levels <- mapM (readLevel . ("levels/"++)) levelFiles

  void . runCurses . flip runStateT initGameState $ do
    w1 <- lift $ uncurry4 newWindow mainWindowVector
    w2 <- lift $ uncurry4 newWindow messageWindowVector

    ans <- lift $ ask w1 w2 "Choose a level: " levelFiles
    when (isJust ans) (initGame w1 w2 (levels !! fromJust ans))

    lift $ closeWindow w1
    lift $ closeWindow w2

initGame :: Window
         -> Window
         -> Board (Board SudokuNumber)
         -> Game ()
initGame w1 w2 level = do
  assign gBoard level 

  lift $ initGameWindow w1 w2
  lift $ renderBoard w1 level
  
  gameLoop w1 w2

gameLoop :: Window
         -> Window
         -> Game ()
gameLoop w1 w2 = do
  b <- use gBoard

  if gameCleared b 
  then gameWin w1 w2
  else do
    lift $ moveFocusTo b       
    input <- lift $ parseInput w1

    case input of
      MoveKey k  -> moveFocus k >> gameLoop w1 w2 
      Quit       -> return ()
      InputNum n -> tryInputNum (Just n) >> gameLoop w1 w2
      Escape     -> tryInputNum Nothing >> gameLoop w1 w2 
      _          -> lift beep >> gameLoop w1 w2
  where
    tryInputNum :: Maybe Number -> Game ()
    tryInputNum n = do
      b <- use gBoard
      let pos1 = b ^. bPosition
          pos2 = b ^. (bgAx pos1 . bPosition) 
      
      case b ^. (bgAx pos1 . bgAx pos2) of
        Left _  -> lift beep
        Right _ ->
          if isJust n 
            then lift $ drawNumber (show $ fromJust n) w1 False pos1 pos2 
            else lift $ drawNumber " " w1 False pos1 pos2

    moveFocusTo (Board b2 pos1) = do
      updateWindow w1 $ do
        let pos2 = (b2!pos1) ^. bPosition
        uncurry moveCursor $ toCoord pos1 pos2  
      render

    updateFocus (Board b2 pos1) k 
      | KUp <- k    = justify coord1 (coord2 & _2 %~ pred)
      | KDown <- k  = justify coord1 (coord2 & _2 %~ succ)
      | KLeft <- k  = justify coord1 (coord2 & _1 %~ pred)
      | KRight <- k = justify coord1 (coord2 & _1 %~ succ)
      where 
        pos2 = (b2!pos1) ^. bPosition

        coord1 = fromPosition pos1
        coord2 = fromPosition pos2

        justify (x1, y1) (x2, y2) =
          let x1' = if' (x2 < 0) (x1 - 1) (if' (x2 >= 3) (x1 + 1) x1)
              y1' = if' (y2 < 0) (y1 - 1) (if' (y2 >= 3) (y1 + 1) y1)
          in  ( fromCoord ((x1' + 3) `mod` 3, (y1' + 3) `mod` 3)
              , fromCoord ((x2 + 3) `mod` 3, (y2 + 3) `mod` 3))
    
    moveFocus k = do 
      b <- use gBoard
      let (pos1, pos2) = updateFocus b k

      gBoard.bPosition             .= pos1
      gBoard.(bgAx pos1).bPosition .= pos2

gameWin :: Window
        -> Window
        -> Game ()
gameWin = (lift.) . gameWinWindow






