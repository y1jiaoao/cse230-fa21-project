{-# LANGUAGE DeriveFunctor #-}
module Model.Board 
  ( -- * Types
    Board
  , XO (..)
  , Pos (..)
  , Result (..)

    -- * Board API
  , dim
  , (!)
  , init
  , put
  , positions
  , emptyPositions
  , boardWinner
  , flipXO

    -- * Moves
  , up
  , down
  , left
  , right
  )
  where

import Prelude hiding (init)
import qualified Data.Map as M 

-------------------------------------------------------------------------------
-- | Board --------------------------------------------------------------------
-------------------------------------------------------------------------------

type Board = M.Map Pos XO

data XO 
  = X 
  | O
  deriving (Eq, Show)

data Pos = Pos 
  { pRow :: Int  -- 1 <= pRow <= dim 
  , pCol :: Int  -- 1 <= pCol <= dim
  }
  deriving (Eq, Ord, Show)

(!) :: Board -> Pos -> Maybe XO 
board ! pos = M.lookup pos board

dim :: Int
dim = 15

positions :: [Pos]
positions = [ Pos r c | r <- [1..dim], c <- [1..dim] ] 

emptyPositions :: Board -> [Pos]
emptyPositions board  = [ p | p <- positions, M.notMember p board]

init :: Board
init = M.empty

-------------------------------------------------------------------------------
-- | Playing a Move
-------------------------------------------------------------------------------
                 
data Result a 
  = Win XO a
  | Draw 
  | Retry 
  | Continue a
  deriving (Eq, Functor, Show)

put :: Board -> XO -> Pos -> Result Board
put board xo pos = case M.lookup pos board of 
  Just _  -> Retry
  Nothing -> result board xo pos

result :: Board -> XO -> Pos -> Result Board
result board xo pos
  | isFull (M.insert pos xo board) = Draw
  | wins (M.insert pos xo board) xo pos = Win xo (M.insert pos xo board)
  | otherwise = Continue (M.insert pos xo board)

wins :: Board -> XO -> Pos -> Bool
wins b xo pos =
  countTowards b xo (Just pos) (Pos 0 1) + countTowards b xo (Just pos) (Pos 0 (-1)) >= 6
  || countTowards b xo (Just pos) (Pos 1 0) + countTowards b xo (Just pos) (Pos (-1) 0) >= 6
  || countTowards b xo (Just pos) (Pos 1 1) + countTowards b xo (Just pos) (Pos (-1) (-1)) >= 6
  || countTowards b xo (Just pos) (Pos 1 (-1)) + countTowards b xo (Just pos) (Pos (-1) 1) >= 6

moveTowards :: Pos -> Pos -> Maybe Pos
moveTowards pos dir
  | newRow < 1 || newRow > dim || newCol < 1 || newCol > dim = Nothing  --- out of board
  | otherwise = Just (Pos newRow newCol)
  where
    newRow = pRow pos + pRow dir
    newCol = pCol pos + pCol dir

countTowards :: Board -> XO -> Maybe Pos -> Pos -> Int
countTowards _ _ Nothing _ = 0  --- moves out of board
countTowards board xo (Just pos) dir
  | board ! pos == Just xo
  = 1 + countTowards board xo (moveTowards pos dir) dir
  | otherwise
  = 0

isFull :: Board -> Bool
isFull b = M.size b == dim * dim

-------------------------------------------------------------------------------
-- | Moves 
-------------------------------------------------------------------------------

up :: Pos -> Pos 
up p = p 
  { pRow = max 1 (pRow p - 1) 
  } 

down :: Pos -> Pos
down p = p 
  { pRow = min dim (pRow p + 1) 
  } 

left :: Pos -> Pos 
left p = p 
  { pCol   = max 1 (pCol p - 1) 
  } 

right :: Pos -> Pos 
right p = p 
  { pCol = min dim (pCol p + 1) 
  } 

boardWinner :: Result a -> Maybe XO
boardWinner (Win xo a) = Just xo
boardWinner _        = Nothing

flipXO :: XO -> XO
flipXO X = O
flipXO O = X

