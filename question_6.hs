import Data.Char
import System.IO

--ensure input filename is same as below
main = do 
  input <- readFile "input.txt"
  putStrLn "Select an index: "
  ind <- getLine
  putStrLn (encode ind input)
  
encode :: String -> String -> String
encode ind str = [shift ind x | x <- str]

--subtract value of a to start from 0
let2Int :: Char -> Int
let2Int x = ord x - ord 'a'

int2Let :: Int -> Char
int2Let x = chr (ord 'a' + x)

--uses mod 26 so it loops round standard 26 letter alphabet
shift :: String -> Char -> Char
shift _ ' ' = ' '
shift ind x = int2Let ( mod ((read ind :: Int) + let2Int x) 26)