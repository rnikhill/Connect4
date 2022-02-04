module Connect4 where

import Common
import Control.Monad (forM_)
import Data.List
import Data.Maybe
import GameTree

eitherUntil :: Monad m => (b -> m (Either a b)) -> b -> m a
eitherUntil action curState = do
  result <- action curState
  case result of
    Left a -> return a
    Right b -> eitherUntil action b

computerMove :: Int -> PlayMove
computerMove evalDepth gameState@(GameState rows cols winLen toPlay board) = getBestMove gameState evalDepth

-- IO
humanMove :: PlayMove
humanMove gameState@(GameState rows cols winLen toPlay board) = do
  putStrLn $ "Player " ++ show toPlay ++ " please insert column to move"
  readLn

genericMove :: PlayMove -> PlayMove -> GameState -> IO (Either (Result, Board) GameState)
genericMove playO playX gameState@(GameState rows cols winLen toPlay board) = do
  putStrLn "========================="
  putStrLn "Current state of board:"
  putStrLn $ show toPlay ++ " to play:"
  printBoard board
  move <- case toPlay of
    O -> playO gameState
    X -> playX gameState
  case applyMove gameState move of
    Nothing -> do
      putStrLn "Invalid move! Please try again"
      return $ Right gameState
    Just success -> return success


readBoard :: IO GameState
readBoard = do
  putStrLn "Please enter number of rows"
  rows <- readLn
  putStrLn "Please enter columns"
  cols <- readLn
  putStrLn "Please enter spaces to win"
  win <- readLn
  return $ mkGame rows cols win

printBoard :: Board -> IO ()
printBoard b = do
  let nCols = length $ head b
  forM_ [0 .. nCols - 1] $ \x -> putStr $ show x ++ " "
  putStrLn ""
  forM_ b $ \x -> do
    forM_ x $ \x' -> do
      putStr $ show x'
      putStr " "
    putStrLn ""

revealWinner :: (Result, Board) -> IO ()
revealWinner (winner, endBoard) = do
  case winner of
    WonBy player -> putStrLn $ "Player " ++ show player ++ " won!"
    Draw -> putStrLn "The result is a draw!"
  printBoard endBoard

playHuman :: IO ()
playHuman = do
  gameState <- readBoard
  (winner, endBoard) <- eitherUntil (genericMove humanMove humanMove) gameState
  revealWinner (winner, endBoard)

playComputer :: IO ()
playComputer = do
  gameState <- readBoard
  putStrLn "Please enter evaluation depth"
  depth <- readLn :: IO Int
  (winner, endBoard) <- eitherUntil (genericMove (computerMove 6) (humanMove)) gameState
  revealWinner (winner, endBoard)
