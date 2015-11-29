{-# LANGUAGE LambdaCase, RankNTypes #-}

module Utils where

import Data.Ix
import Data.Array

import Control.Lens
import Control.Monad

import UI.NCurses

import Types

uncurry4 f (a, b, c, d) = f a b c d

posRange = (Position T L, Position B R)
allPos   = range posRange

ax :: Ix i => i -> Lens' (Array i a) a 
ax i = lens (!i) (\arr v -> arr // [(i, v)])

bgAx i = bGrid . (ax i)

fromCoord :: (Integer, Integer) -> Position
fromCoord coord
  = let (x, y) = coord & both %~ fromInteger
    in  Position (toEnum y) (toEnum x)

fromPosition :: Position -> (Integer, Integer)
fromPosition (Position v h)
  = (fromEnum h, fromEnum v) & both %~ fromIntegral

drawStringB :: String -> Update ()
drawStringB s = do
  setAttribute AttributeBold True
  drawString s
  setAttribute AttributeBold False 

parseInput :: Window -> Curses (Types.Event)
parseInput w = do
  setEcho False

  getEvent w Nothing >>= \case
    Just k  -> parseEvent k
    Nothing -> parseInput w
  where
    parseEvent (EventSpecialKey KeyUpArrow)    = return (MoveKey KUp)
    parseEvent (EventSpecialKey KeyDownArrow)  = return (MoveKey KDown)
    parseEvent (EventSpecialKey KeyLeftArrow)  = return (MoveKey KLeft)
    parseEvent (EventSpecialKey KeyRightArrow) = return (MoveKey KRight)

    parseEvent (EventCharacter c)
      | c == 'q'            = return Quit
      | c `elem` ['1'..'9'] = return (InputNum $ read [c])
      | c == '\n'           = return Return
      | c == '\DEL'         = return Escape

    parseEvent _ = beep >> parseInput w

initBoard arr
  = Board arr (Position T L)

initGameState
  = GameState undefined

readLevel :: String -> IO (Board (Board SudokuNumber))
readLevel f = do
  content <- readFile f 
  
  let parsed = read content :: [[SudokuNumber]]

  when (length parsed /= 9) $ fail "invalid level file"

  let buildArr = listArray (Position T L, Position B R)
      blocks   = (initBoard . buildArr) `map` parsed 

  return $ (initBoard . buildArr) blocks

if' :: Bool -> a -> a -> a
if' True t _  = t
if' False _ e = e
