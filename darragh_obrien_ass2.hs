import Data.List
--Question 1
--is_square :: Int -> Bool
is_square x
  | x < 1       = False
  | x == 1      = True
  | otherwise   = is_square (x `div` 2)
  
--Question 2
--freq_letter_pc xs = [(x,length xs) | x <- xs , x /= ' ']
--freq_letter_pc xs = [(x,length xs) | x <- xs , x /= ' ']
freq_letter_pc xs = nub xs

--Question 3
--id, name, population, country_id
cities=[(1,"Paris",7000000,1),(2,"London",8000000,2),(1,"Rome",3000000,3),(1,"Edinburgh",500000,2),(1,"Florence",50000,3),(1,"Venice",200000,3), (1,"Lyon",1000000,1),(1,"Milan",3000000,3),(1,"Madrid",6000000,4),(1,"Barcelona",5000000,4)]
--id, country
countries=[(1,"UK"),(2,"France"),(3,"Italy"),(4,"Spain")]
-- 3.A
get_city_above :: Int -> [String]
get_city_above x = [y | (_,y,z,_) <- cities, z >= x]
-- 3.B
get_city :: String -> [String]
get_city x = [city | (id, country) <- countries, (_, city, _, country_id) <- cities, x == country && id == country_id]
-- 3.C
num_city :: [(String, Int)]
num_city = [(country, length (get_city country)) | (_, country) <- countries]

--Question 4
eucl_dist :: [Float] -> [Float] -> Float
eucl_dist xs ys = sqrt (sum [(x - y) ^ 2 | (x,y) <- zip xs ys])