import System.IO
import Data.List
import Data.Char

--takes an input str. loops over each character and makes it lowercase
toLowerStr :: String -> String
toLowerStr xs = map toLower xs

--makes string lowercase
--loops through each character to remove non alpha characters
--splits string into lists
--gets unique values in list
get_words :: String -> [String]
get_words = nub . words . (filter (\x -> isLower x || isSpace x)) . toLowerStr

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
  let dict =  nub (ulysses_words ++ dorian_words ++ pride_words)
  writeFile "dict1.txt" (show dict)
  putStrLn "Dictionary created"