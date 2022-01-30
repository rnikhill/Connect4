module Common where

import Data.List
import Data.Maybe

data Player = O | X deriving (Eq, Show)

data Square = FilledBy Player | B deriving Eq

data Result = WonBy Player | Draw deriving (Eq, Show)

type Row = [Square]

type Board = [Row]

type PlayMove = GameState -> IO (Either (Result, Board) GameState)
-- match gameState@(GameState rows cols winLen toPlay board)
data GameState = GameState Int Int Int Player Board deriving (Eq, Show)

instance Show Square where
  show B = "."
  show (FilledBy p) = show p

instance Ord Player where
  compare O X = LT
  compare X O = GT
  compare _ _ = EQ

instance Ord Result where
  compare (WonBy p1) (WonBy p2) = p1 `compare` p2
  compare (WonBy O) Draw = LT
  compare (WonBy X) Draw = GT
  compare Draw (WonBy O) = GT
  compare Draw (WonBy X) = LT
  compare _ _ = EQ

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer _ = X

mkGame :: Int -> Int -> Int -> GameState
mkGame rows cols win = GameState rows cols win O [row | i <- [1 .. rows]]
  where
    row = [B | j <- [1 .. cols]]

diagonalizeInc :: Board -> Board
diagonalizeInc b =
  let numPrepend = [0 .. (length b - 1)]
      prefix = map (\n -> [B | _ <- [1 .. n]]) numPrepend
      zipCat = zipWith (++)
   in transpose $ prefix `zipCat` b `zipCat` reverse prefix

diagonalizeDec :: Board -> Board
diagonalizeDec b = diagonalizeInc $ map reverse b

isWinner :: Board -> Int -> Player -> Bool
isWinner board winLen player =
  let winStr = replicate winLen (FilledBy player)
      rows = board
      cols = transpose board
      diag1 = diagonalizeInc board
      diag2 = diagonalizeDec board
      hasWin x = any (isInfixOf winStr) x
   in any hasWin [rows, cols, diag1, diag2]

-- isFull
isDraw :: Board -> Bool
isDraw = all isFull where
  isFull row = isNothing (elemIndex B row)

replaceAt :: Int -> a -> [a] -> [a]
replaceAt idx elem lst = take idx lst ++ [elem] ++ drop (idx + 1) lst

makeMove :: Int -> Player -> Board -> Maybe Board
makeMove col player board =
  let cols = transpose board
      validCol = col < length cols
      selected = cols !! col
      blanks = elemIndices B selected
      validMove = not $ null blanks
      newCol = replaceAt (last blanks) (FilledBy player) selected
      newCols = replaceAt col newCol cols
   in if validCol && validMove then Just (transpose newCols) else Nothing

applyMove :: GameState -> Int -> Maybe (Either (Result, Board) GameState)
applyMove gameState@(GameState rows cols winLen toPlay board) col = do
  board' <- makeMove col toPlay board
  if isWinner board' winLen toPlay then return $ Left (WonBy toPlay, board')
  else if isDraw board' then return $ Left (Draw, board')
  else return $ Right (GameState rows cols winLen (nextPlayer toPlay) board')