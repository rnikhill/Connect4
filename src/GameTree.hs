module GameTree where

import Common
import Data.List

data GameTree = Leaf (Result, Board) | Node GameState [(Int, GameTree)]

data MinimaxTree = MinimaxTree Result [(Int, MinimaxTree)]

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

getBestMove :: GameState -> Int -> Int
getBestMove gameState@(GameState rows cols winLen toPlay board) evalDepth =
  let tree = makeGameTree gameState
   in case minimax evalDepth tree of
        MinimaxTree result children ->
          let moves = map fst children `zip` getResults children
              res = find ((== result) . snd) moves
           in case res of
                Nothing -> 0
                Just (col, _) -> col