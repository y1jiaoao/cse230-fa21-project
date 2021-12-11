module Model.Player where

import Model.Board
import System.Random -- (Random(randomRIO))
import qualified Data.Map as M 

-------------------------------------------------------------------------------
-- | Players and Strategies ---------------------------------------------------
-------------------------------------------------------------------------------

data Player = Player 
  { plName  :: String 
  , plStrat :: Strategy
  } 

type Strategy = Pos     -- ^ current cursor
             -> Board   -- ^ current board
             -> XO      -- ^ naught or cross
             -> IO Pos  -- ^ next move

human :: Player 
human = Player "human" (\p _ _ -> return p)

rando :: Player 
rando = Player "machine" weakStrategy

randomStrategy :: a -> Board -> b -> IO Pos
randomStrategy _ b _ = selectRandom (emptyPositions b) 

selectRandom :: [a] -> IO a
selectRandom xs = do
  i <- randomRIO (0, length xs - 1)
  return (xs !! i)

weakStrategy :: a -> Board -> b -> IO Pos
weakStrategy _ b _ = selectWeak b (emptyPositions b) 

selectWeak :: Board -> [Pos] -> IO Pos
selectWeak b xs = do
  let re = (selectWeak2 b xs (length xs - 1))
  i <- randomRIO (0, length re - 1)
  let (_,pos) = re !! i
  return pos

selectWeak2 :: Board -> [Pos] -> Int -> [(Int,Pos)]
selectWeak2 b xs 0 = [((boardScore b (xs!!0)),(xs!!0))]
selectWeak2 b xs n 
  | maxscore < score = [(score,pos)]
  | maxscore ==score = (score,pos):re
  | otherwise        = re
  where re = selectWeak2 b xs (n-1)
        (maxscore,mpos) = head re
        score = boardScore b pos
        pos = (xs!!n)

boardScore :: Board -> Pos -> Int
boardScore b pos = (scoreFor5 newbO O 99999)+(scoreFor5 newbX X 50000)
                  +(scoreFor4 newbO O 5000)+(scoreFor4 newbX X 2000)
                  +(scoreFor3 newbO O 100)+(scoreFor3 newbX X 20)
                  +(scoreFor2 newbO O 2)+(scoreFor2 newbX X 1)
  where newbO = (M.insert pos O b)
        newbX = (M.insert pos X b)

scoreFor5 :: Board -> XO -> Int -> Int
scoreFor5 b xo r = (patternExistTimes b xo 5) * r

scoreFor4 :: Board -> XO -> Int -> Int
scoreFor4 b xo r = (patternExistTimes b xo 4) * r

scoreFor3 :: Board -> XO -> Int -> Int
scoreFor3 b xo r = (patternExistTimes b xo 3) * r

scoreFor2 :: Board -> XO -> Int -> Int
scoreFor2 b xo r = (patternExistTimes b xo 2) * r

patternExistTimes :: Board -> XO -> Int -> Int
patternExistTimes b xo n = sum [ pPoss b xo ps | ps <- patterns n]

pPoss :: Board -> XO -> [Pos] -> Int
pPoss b xo ps = if and [ b!p == Just xo | p <- ps ] then 1 else 0

patterns :: Int -> [[Pos]]
patterns n = (rowsn n) ++ (colsn n) ++ (diagsn n) 

rowsn, colsn, diagsn :: Int -> [[Pos]]
rowsn n = rowsList (dim-n+1)
  where 
    rowsList 0 = []
    rowsList i = [[Pos r c | c <- [i..i+n-1]] | r <- [1..dim]] ++ (rowsList (i-1))
colsn n = colsList (dim-n+1)
  where 
    colsList 0 = []
    colsList i = [[Pos r c | r <- [i..i+n-1]] | c <- [1..dim]] ++ (colsList (i-1))
diagsn n= diagsList (dim-n+1)
  where 
    diagsList 0 = []
    diagsList i = [[Pos r (r+j) | r <- [i..i+n-1]] | j <- [(1-i)..(2+dim-n-i)]]++[[Pos r (n+1-r+j) | r <- [i..i+n-1]] | j <- [(i-1)..(i+dim-n-1)]] ++ (diagsList (i-1))
