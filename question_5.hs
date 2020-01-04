import Data.Char
import Data.List

test = "This is a long bit of test text that I want to detect the language it is written in. I hope that it works correctly, if not then it doesn't work"
  
eng_freq :: [Float]
eng_freq = [8.12,1.49,2.71,4.32,12.02,2.30,2.03,5.92,7.31,0.10,0.69,3.98,2.61,6.95,7.68,1.82,0.11,6.02,6.28,9.10,2.88,1.11,2.09,0.17,2.11,0.07]

pt_freq :: [Float]
pt_freq = [12.21, 1.01, 3.35, 4.21, 13.19, 1.07, 1.08, 1.22, 5.49, 0.30, 0.13, 3.00, 5.07, 5.02, 10.22, 3.01, 1.10, 6.73, 7.35, 5.07, 4.46, 1.72, 0.05, 0.28, 0.04, 0.45]

--remove all occurences of an element from a list
remove ::Eq a => a -> [a] -> [a]
remove x list = [y | y <- list, y /= x]

--count the number of times an element appears in a list
num_occ :: Eq a => a -> [a] -> Int
num_occ x [] = 0
num_occ x (y:ys)
   | x==y = 1 + (num_occ x ys)
   | otherwise = num_occ x ys

freq_letter_pc :: String -> [(Float, Char)]
freq_letter_pc list = let new_list = map toLower (remove ' ' list)
                      in [(fromIntegral (num_occ y new_list) / fromIntegral (length new_list), y) | y <- nub new_list, elem y ['a'..'z'] ]