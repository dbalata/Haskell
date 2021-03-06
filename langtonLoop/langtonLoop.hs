import Data.Array
import Data.Char
import Transition
import System.Console.ANSI
import Control.Concurrent

startLoop = ["022222222000000",
             "217014014200000",
             "202222220200000",
             "272000021200000",
             "212000021200000",
             "202000021200000",
             "272000021200000",
             "212222221222220",
             "207107107111112",
             "022222222222220"]

offset = 10
startLoopWidth = 15
startLoopHeight = 10
maxDim = 55

main = do
  let init = placeStartLoop $ array ((0, 0),(maxDim,maxDim)) [((x, y), '0') |
                                             x <- [0..maxDim], y <- [0..maxDim]]
  clearScreen
  updateLoop init 100000

updateLoop array numGens = do
  setCursorPosition 0 0
  let updated = update array
  printArray updated
  if numGens /= 0 then updateLoop updated (numGens - 1)
  else return ()
  

getRow n a =
  [a ! (n, c) | c <- [minCol..maxCol]] where ((_, minCol), (_, maxCol)) = bounds a

printRow n a = do print $ getRow n a

printArray a = do
  mapM_ (\x -> printRow x a) [minRow..maxRow] where ((minRow, _), (maxRow, _)) = bounds a
  
placeStartLoop a = a // changeList
  where changeList = [((x + offset, y + offset), (startLoop !! x) !! y) |
                    x <- [0..startLoopHeight-1], y <- [0..startLoopWidth-1]]

getFromArray (y,x) a =
  a ! (row, col) where
  row = y `mod` maxDim
  col = x `mod` maxDim
 
getTableEntry (x,y) a =
  getFromArray (x+0,y+0) a :
  getFromArray (x-1,y+0) a :
  getFromArray (x+0,y+1) a :
  getFromArray (x+1,y+0) a :
  getFromArray (x+0,y-1) a : []

update a =  a // changeList
  where changeList = [((x, y), getTransition (getTableEntry (x, y) a)) |
                       x <- [0..maxDim], y <- [0..maxDim]]
