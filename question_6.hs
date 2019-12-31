import Data.Char
import System.IO

--ensure input filename is same as below
main = do 
  input <- readFile "input.txt"
  scrm <- getLine
  putStrLn (encode scrm input)
  
encode :: String -> String -> String
encode scrm str = [ shift scrm x | x <- str  ]

--subtract value of a to start from 0
let2Int :: Char -> Int
let2Int x = ord x - ord 'a'

int2Let :: Int -> Char
int2Let x = chr (ord 'a' + x)

--uses mod 26 so it loops round standard 26 letter alphabet
shift :: String -> Char -> Char
shift scrm x = int2Let ( mod ((read scrm :: Int) + let2Int x) 26)