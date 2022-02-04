module GameTree where

import Common
import Data.List
import System.Random

data GameTree = Leaf (Result, Board) | Node GameState [(Int, GameTree)]

data MinimaxTree = MinimaxTree Result [(Int, MinimaxTree)]

findRandomElement :: (a -> Bool) -> [a] -> IO (Maybe a)
findRandomElement f xs = do
  let indices = findIndices f xs
  indInd <- randomRIO (0, length indices - 1)
  let index = indices !! indInd
  case indices of
    [] -> return Nothing
    _ -> return $ Just $ xs !! index

makeGameTree :: GameState -> GameTree
makeGameTree gameState@(GameState rows cols winLen toPlay board) =
  let possibleMoves = [0 .. cols - 1]
      nextStates = possibleMoves `zip` map (applyMove gameState) possibleMoves
      validStates = [(col, x) | (col, Just x) <- nextStates]
      go (col, state) = (col, tree)
        where
          tree = case state of
            Left leafState -> Leaf leafState
            Right nodeState -> makeGameTree nodeState
   in Node gameState $ map go validStates

minimax :: Int -> GameTree -> MinimaxTree
minimax evalDepth gameTree = case gameTree of
  Leaf (result, _) -> MinimaxTree result []
  Node gameState@(GameState rows cols winLen toPlay board) children ->
    if evalDepth <= 0
      then MinimaxTree Draw []
      else case children of
        [] -> MinimaxTree Draw [] --Shouldn't happen! but
        _ -> case toPlay of
          O -> MinimaxTree (minimum results) childRes
          X -> MinimaxTree (maximum results) childRes
    where
      childRes = map fst children `zip` map (minimax (evalDepth - 1) . snd) children
      results = [result | (_, MinimaxTree result _) <- childRes]

getResults :: [(Int, MinimaxTree)] -> [Result]
getResults xs = [result | (_, MinimaxTree result _) <- xs]

getBestMove :: GameState -> Int -> IO Int
getBestMove gameState@(GameState rows cols winLen toPlay board) evalDepth = do
  let tree = makeGameTree gameState
  case minimax evalDepth tree of
    MinimaxTree result children -> do
      let moves = map fst children `zip` getResults children
      res <- findRandomElement ((== result) . snd) moves
      case res of
        Nothing -> return 0
        Just (col, _) -> return col