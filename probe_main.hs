-- |
-- Module      : probe_main
-- License     : Freeware
-- Maintainer  : Denis Davydoff (dendavydoff@gmail.com)
-- Stability   : Stable
-- Portability : Excellent
--
-- Test for Sudoku solver
--

import Sudoku 

mat = [[ 0, 0, 0, 0, 0, 0, 4, 0, 0], [ 3, 0, 6, 0, 0, 0, 0, 0, 0], [ 0, 0, 0, 1, 9, 6, 0, 3, 0], [ 0, 7, 0, 0, 0, 0, 0, 1, 0], [ 8, 0, 0, 2, 5, 0, 0, 9, 0], [ 0, 4, 0, 0, 0, 0, 8, 0, 0], [ 0, 6, 0, 4, 0, 9, 0, 0, 8], [ 0, 0, 5, 0, 0, 0, 0, 2, 0], [ 0, 0, 0, 5, 0, 0, 0, 0, 7]]::MatInt2d

main :: IO ()
main =  do
putStrLn ( print_mat2d mat )
putStrLn ( print_mat2d ( solver mat ) )

