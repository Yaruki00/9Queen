import Data.List
import Data.Function
import Control.Monad

type Cell = (Int, Int)
type Board = [(Cell, Bool)]

cells :: [Cell]
cells = [ (x, y) | x <- [0..8], y <- [0..8] ]

isActive :: Board -> Cell -> Bool
isActive [] _ = True
isActive (queen:queens) cell
         | (fst . fst) queen == fst cell                                                = False
         | (snd . fst) queen == snd cell                                                = False
         | abs ( (fst . fst) queen - fst cell ) == abs ( (snd . fst) queen - snd cell ) = False
         | otherwise                                                    = isActive queens cell

solve :: Board -> [Board]
solve board 
         | length board == 9 = [board]
solve board = [ (cell, exist) : board
              | let remains = cells \\ map fst board
              , cell <- remains
              , let exist = isActive board cell
              , exist == True
              ] >>= solve

main :: IO()
main = case solve problem of
    answer : _ -> print answer
    []         -> putStrLn "invalid problem"

problem :: Board
problem = []