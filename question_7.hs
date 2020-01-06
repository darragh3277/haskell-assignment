import System.IO
import Data.List
import Data.Char

--takes an input str. loops over each character and makes it lowercase
toLowerStr :: String -> String
toLowerStr xs = map toLower xs

--count the number of times an element appears in a list
num_occ :: Eq a => a -> [a] -> Int
num_occ x [] = 0
num_occ x (y:ys)
   | x==y = 1 + (num_occ x ys)
   | otherwise = num_occ x ys
   
--takes two inputs, a dictionary list dict and a list of words to check
--returns the number of times a word in xs appears in the dictionary
total_words :: [String] -> [String] -> Int
total_words xs dict = sum [num_occ x dict | x <- xs]

--makes string lowercase
--loops through each character to remove non alpha characters
--splits string into lists
--gets unique values in list
get_words :: String -> [String]
get_words = nub . words . (filter (\x -> isLower x || isSpace x)) . toLowerStr

--subtract value of a to start from 0. eg a = 0 and not 97
let2Int :: Char -> Int
let2Int x = ord x - ord 'a'

int2Let :: Int -> Char
int2Let x = chr (ord 'a' + x)

--shift an input string by x chars
shift :: String -> Int -> String
shift input i = [ int2Let (mod ((let2Int x) + i) 26) | x <- input]

--shift a list of words by x chars
shift_words :: [String] -> Int -> [String]
shift_words ws i = [shift x i | x <- ws]

--takes an input of words to test for occurances
--shifts each char by +1 ind 
--compares words each shift to the input dictionary
--returns a tuple of (occurances of words, shifted index)
get_shift_freq :: [String] -> [String] -> [(Int, Int)]
get_shift_freq inp dict = [(total_words (shift_words inp x) dict, x) | x <- [0..25]]

--takes tuple input (occurances of words, shifted index)
--returns index
index :: (Int, Int) -> Int
index (_,x) = x

--Note, this is a slow process
--builds the dictionary and writes it to dict.txt
--first read the input of the source files
--get all unique words of source files
--join the lists and get the unique words from combined source files
--write to dict.txt
--output when complete
build_dictionary :: IO ()
build_dictionary = do 
  ulysses <- readFile "ulysses.txt"
  dorian <- readFile "dorian.txt"
  pride <- readFile "pride.txt"
  let ulysses_words = get_words ulysses
  let dorian_words = get_words dorian
  let pride_words = get_words pride
  let dict =  unwords (nub (ulysses_words ++ dorian_words ++ pride_words))
  writeFile "dict.txt" (show dict)
  putStrLn "Dictionary created"
  
--Note, this is a slow process
--Reads the input in dit.txt.chp. Creates a list of unique words form the input
--Reads input in dict.txt and creates a list of the words in it
--highest_index shifts the index 26 times. It creates a sorted tuple list with the (occurances of words, shifted index)
--it then takes the shifted index in the last tuple
--decoded_words shifts all the encrypted words to the index provided by highest_index
--decoded_str just converts the list back to a string
--We then write the output to decoded.txt and print a message saying we are done.
guess_index :: IO ()  
guess_index = do 
  msg <- readFile "dit.txt.chp"
  dict <- readFile "dict.txt"
  let msg_words = get_words msg
  let dict_list = words dict
  let highest_index = index (last (sort (get_shift_freq msg_words dict_list)))
  let decoded_words = shift_words msg_words highest_index
  let decoded_str = unwords decoded_words
  writeFile "decoded.txt" (show decoded_str)
  putStrLn "Message decoded."
  
  
  
  
  
  
  
  