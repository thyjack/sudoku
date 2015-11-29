{-# LANGUAGE LambdaCase #-}

module Draw where

import Data.Ix

import Control.Monad
import Control.Lens

import UI.NCurses

import Types
import Utils

offset = (2, 4)

mainWindowVector    = (21, 49, 2, 4)
messageWindowVector = (10, 25, 13, 55)
gameWinWindowVector = (9, 37, 8, 30)

drawGrid :: Integer
         -> Integer
         -> (Integer, Integer)
         -> Update ()
drawGrid nV nH (y, x) = do
  let lenV = 3 * nV + 2
      lenH = 3 * nH + 2 

      nLines n c = map (\i -> i * n + (i - 1) + c) [1..]

  -- draw two horizontal lines
  forM_ (take 2 (nLines nV y)) $ \n -> do
    moveCursor n x
    drawLineH Nothing lenH

  -- draw two vertical lines
  forM_ (take 2 (nLines nH x)) $ \n -> do
    moveCursor y n
    drawLineV Nothing lenV

  -- draw crosses
  forM_ [(xs, ys) | ys <- take 2 (nLines nH x), xs <- take 2 (nLines nV y)] 
    $ \coord -> do
        uncurry moveCursor coord
        drawString "┼"

drawBoard' :: (Integer, Integer) -> Update ()
drawBoard' (y, x) = do
  drawGrid 5 13 (y, x) 

  forM_ [(xs, ys) | ys <- [1..3], xs <- [1..3]] $ \(ix, iy) -> 
    drawGrid 1 3 (y + 6 * (iy - 1), x + (2 * ix - 1) + 12 * (ix - 1)) 

drawBoard :: Update ()
drawBoard = drawBoard' offset 

toCoord' :: Position 
         -> Position 
         -> (Integer, Integer)
         -> (Integer, Integer)
toCoord' pos1 pos2 (y, x)
  = let (xo, yo) = fromPosition pos1 & both %~ succ 
        (xi, yi) = fromPosition pos2 & both %~ succ
        
        xo' = x + (2 * xo - 1) + 12 * (xo - 1)
        xi' = 4 * xi - 3
        yo' = y + 6 * (yo - 1)
        yi' = 2 * yi - 2 
    in  (yo' + yi', xo' + xi')

toCoord :: Position -> Position -> (Integer, Integer)
toCoord pos1 pos2 = toCoord' pos1 pos2 offset

ask :: Window
    -> Window
    -> String
    -> [String] -- options
    -> Curses (Maybe Int)
ask w1'' w2'' question options = do
  initWindow w1'' w2''
  drawQuestion w1''

  askLoop w1'' w2'' 0
  where
    size     = length options
    anchors  = iterate nextAnchor $ (4, 4)

    nextAnchor = _1 +~ 2 :: (Integer, Integer) -> (Integer, Integer)
    textAnchor = _2 +~ 1 :: (Integer, Integer) -> (Integer, Integer)

    drawQuestion w = do
      updateWindow w $ do
        uncurry moveCursor (2, 4) 
        drawString question 

    initWindow w1 w2 = do
      updateWindow w1 $ do
        clear; drawBox Nothing Nothing

        forM_ (zip options anchors) $ \(option, anchor) -> do
          uncurry moveCursor anchor 
          drawString $ "[ ] " ++ option
      
      updateWindow w2 $ do
        clear; drawBox Nothing Nothing

        moveCursor 2 2
        drawString "↑: move up"
        moveCursor 3 2
        drawString "↓: move down"

        moveCursor 5 2
        drawString "⏎: answer"
        moveCursor 6 2
        drawString "q: exit"

      render

    cleanSelection w i = do
      updateWindow w $ do
        uncurry moveCursor $ textAnchor (anchors !! i)
        drawString " "
      render

    cleanWindow w = do
      updateWindow w clear
      render

    askLoop w1 w2 i' = do
      let i = (i' + size) `mod` size 
      
      updateWindow w1 $ do
        uncurry moveCursor $ textAnchor (anchors !! i)
        drawString "■"
        uncurry moveCursor $ textAnchor (anchors !! i)
      render

      let parseIn w1' w2' =
            parseInput w1' >>= \case
              MoveKey KUp   -> cleanSelection w1' i >> askLoop w1' w2' (i-1)
              MoveKey KDown -> cleanSelection w1' i >> askLoop w1' w2' (i+1)
              Return        -> mapM_ cleanWindow [w1', w2'] >> return (Just i)
              Quit          -> mapM_ cleanWindow [w1', w2'] >> return Nothing
              _             -> parseIn w1' w2'
      
      parseIn w1 w2

drawNumber :: String
           -> Window
           -> Bool
           -> Position
           -> Position
           -> Curses ()
drawNumber n w bold pos1 pos2 = do
  updateWindow w $ do
    uncurry moveCursor $ toCoord pos1 pos2 
    
    when bold       $ drawStringB n
    when (not bold) $ drawString  n
  render

initGameWindow :: Window
               -> Window
               -> Curses ()
initGameWindow w1 w2 = do
  updateWindow w1 $ do
    clear; drawBox Nothing Nothing
    drawBoard
  updateWindow w2 $ do
    clear; drawBox Nothing Nothing

    moveCursor 2 2
    drawString "↑/↓/←/→: move"

    moveCursor 3 2
    drawString "0-9: input number"

    moveCursor 4 2
    drawString "bs: delete number"

    moveCursor 5 2
    drawString "q: quit"

    moveCursor 7 2
    drawString "number: "
    moveCursor 7 10
    drawStringB "fixed"
    moveCursor 7 15
    drawString "/normal"

  render

renderBoard :: Window
            -> Board (Board SudokuNumber)
            -> Curses ()
renderBoard w board = 
  forM_ (range (Position T L, Position B R)) $ \pos1 ->
    forM_ (range (Position T L, Position B R)) $ \pos2 ->
      case board ^. (bgAx pos1) ^. (bgAx pos2) of
        Left n         -> drawNumber (show n) w True pos1 pos2
        Right (Just n) -> drawNumber (show n) w False pos1 pos2
        _              -> return ()

gameWinWindow :: Window
              -> Window
              -> Curses ()
gameWinWindow _ _ = do
  w3 <- uncurry4 newWindow gameWinWindowVector
  
  updateWindow w3 $ do
    drawBox Nothing Nothing

    moveCursor 3 4
    drawStringB "YOU WON!"

    moveCursor 5 4
    drawString "Press anykey to exit..."
  render

  getEvent w3 Nothing

  closeWindow w3
