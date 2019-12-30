import Data.List
import Data.Char

--helper functions
--count the number of times an element appears in a list
num_occ :: Eq a => a -> [a] -> Int
num_occ x [] = 0
num_occ x (y:ys)
   | x==y = 1 + (num_occ x ys)
   | otherwise = num_occ x ys
--remove all occurences of an element from a list
remove ::Eq a => a -> [a] -> [a]
remove x list = [y | y <- list, y /= x]

--Question 1
--is_square :: Int -> Bool
--4 is the lowest square, so start at 2. increment from 2 to input squaring each int. if int 
--equals the input is a square. once it goes above the input it's not a square. Not super efficient but gets the job done
is_square :: Int -> Bool
is_square x = chk 2 where
  chk n | n * n < x = chk (n + 1)
        | n > x = False
        | n * n == x = True
        | otherwise = False
  
--Question 2
--remove spaces from string and convert to lower case first
--iterate through unique elements, count occurences and divide by lenth of string
freq_letter_pc :: String -> [(Float, Char)]
freq_letter_pc list = let new_list = map toLower (remove ' ' list)
                      in [(fromIntegral (num_occ y new_list) / fromIntegral (length new_list), y) | y <- nub new_list, elem y ['a'..'z'] ]

--Question 3
--id, name, population, country_id
cities=[(1,"Paris",7000000,1),(2,"London",8000000,2),(1,"Rome",3000000,3),(1,"Edinburgh",500000,2),(1,"Florence",50000,3),(1,"Venice",200000,3), (1,"Lyon",1000000,1),(1,"Milan",3000000,3),(1,"Madrid",6000000,4),(1,"Barcelona",5000000,4)]
--id, country
countries=[(1,"UK"),(2,"France"),(3,"Italy"),(4,"Spain")]
--3.A
get_city_above :: Int -> [String]
get_city_above x = [y | (_,y,z,_) <- cities, z >= x]
--3.B
get_city :: String -> [String]
get_city x = [city | (id, country) <- countries, (_, city, _, country_id) <- cities, x == country && id == country_id]
--3.C
num_city :: [(String, Int)]
num_city = [(country, length (get_city country)) | (_, country) <- countries]

--Question 4
eucl_dist :: [Float] -> [Float] -> Float
eucl_dist xs ys = sqrt (sum [(x - y) ^ 2 | (x,y) <- zip xs ys])

--Question 5