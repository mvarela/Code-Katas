import Data.List
import System.Random

--import Data.Ord
-- problem 8 - compressing lists

compress :: Eq(a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs) 
    | x==y = compress (x:xs)
    | otherwise = x:compress (y:xs)


-- problem 9 - pack lists into sublists of consecutive elements


repLen :: Eq(a) => a -> [a] -> Int
repLen _ [] = 0
-- repLen x [y] 
--        | x == y = 1
--        | otherwise = 0
repLen x (y:ys)
    | x == y = 1 + repLen x ys
    | otherwise = 0



pack :: Eq(a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs)
         | x /= head xs = [[x]] ++ pack xs
         | otherwise = [take (repLen x (x:xs)) (x:xs)] ++ pack (drop (repLen x (x:xs)) (x:xs))


-- problem 10, run lenght encoding
rle :: Eq(a) => [a] -> [(Int,a)]
rle xs = zip (map length (pack xs)) (map head (pack xs))

-- problem 11, modified RLE

data RLEUnit a = Single a | Multiple Int a deriving(Show, Eq)

rle' :: Eq(a) => [a] -> [RLEUnit a]
rle' xs = map conv (rle xs) 
  where conv (1, dat) = Single dat
        conv (x, dat) = Multiple x dat
        
        
-- Problem 12, decode a RLN-encoded list


unpackRLE :: RLEUnit a -> [a]
unpackRLE (Single x) = [x]
unpackRLE (Multiple i x) = take i (repeat x)

decode' :: Eq(a) => [RLEUnit a] -> [a]
decode' = foldl (++) []  . (map unpackRLE)

-- Problem 13  skipped

-- Problem 14 Duplicate elements of a list

duplicate :: [a] -> [a]
--duplicate = foldl (++) [] . map (take 2 . repeat)
duplicate = concatMap $ replicate 2
                  
-- Problem 15 Replicate the elements of a list a given number of times

repli ::  [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs


-- Problem 16 Drop the Nth element from a list

dropNth :: [a] -> Int -> [a]
dropNth xs n = filterNth 1 n xs
  where filterNth :: Int -> Int -> [a] -> [a]
        filterNth _ _ [] = []
        filterNth m n (x:xs) = if (m==n) 
                               then 
                                 (filterNth 1 n xs)  
                               else  
                                 (x : filterNth (m+1) n xs)
                               
-- Problem 17 split a list into two parts, without using built-in functions

splitN :: [a] -> Int -> ([a] , [a])
splitN xs n = ((myTake n xs) , (myDrop n xs))
  where
    myTake :: Int -> [a] -> [a]
    myTake _ [] = []
    myTake 0 _ = []
    myTake m (x:xs) = x:(myTake (m-1) xs)
    myDrop :: Int -> [a] -> [a]
    myDrop _ [] = []
    myDrop 0 xs = xs
    myDrop m (x:xs) = myDrop (m-1) xs
    
-- Problem 18 Take a slice from a list

-- this could be improved by validating the parameters and returning Maybe [a]
slice :: [a] -> Int -> Int -> [a]
slice xs i j = drop (i-1) $ take j xs

-- Problem 19 Rotate a list by a given number of positions (can be negative)

rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0 = xs
rotate xs n 
  | n > 0 = drop (n `mod` (length xs)) xs ++ take (n `mod` (length xs)) xs
  | n < 0 = reverse $ rotate (reverse xs) (-n)
            
-- Problem 20 Remove the Kth element from a list

removeAt :: Int -> [a] -> [a]
removeAt k xs = map snd $
                filter (\ (x,_) -> x /= k) $
                zip [1..] xs

-- Problem 21 insert an element in a list at a given position

-- this solution just adds to the end or to the beginning if the position does 
-- not exist in the list

insertAt :: a -> [a] -> Int -> [a]
insertAt y xs k = take (k-1) xs ++ y:(drop k xs)

-- Problem 22 Create a list containing all integers in a given range

range :: Int -> Int -> [Int]
range x y 
  | x <= y = [x..y]
  | x > y = reverse [y..x]
            
            
-- Problem 23 extract a given number of randomly selected elements from a list

-- Make it use the number of extracted elements as a seed. We could add an extra
-- argument as an explicit seed

seeds s = (randoms $ mkStdGen s)::[Int]

extractNRandom :: (Eq a) => [a] -> Int -> [a]
extractNRandom l n = go rands [] n l
               where
                rands = take n $ seeds n
                go :: (Eq a) => [Int] -> [a] -> Int -> [a] -> [a]
                go r t i s 
                   | i == 0 = t
                   | s == [] = t
                   | otherwise = let 
                                     e = (!!) s $ abs $ head r `rem` (fromIntegral $ length s)
                                 in
                                     go  (tail r)  (e:t)  (i - 1) $ delete e s
                                                     


-- Problem 24 Lotto: Draw N different random numbers from the set 1..M. Seeding
-- as above

extractNRandom1M :: Int -> Int -> [Int]
extractNRandom1M n m 
                 | n == 0 = []
                 | otherwise = go rands n [1..m]
                   where 
                         rands = take n $ seeds n
                         go :: [Int] -> Int -> [Int] -> [Int]
                         go _ 0 _ = []
                         go r count src = (:) e $ go (tail r) (count - 1) $ delete e src
                            where
                                e = (!!) src $ abs $ head r `rem` (fromIntegral $ length src)           


-- Problem 25 Generate a random permutation of the elements of a list. 
permutation :: [a] -> [a]
permutation xs = map ((xs!!) . (flip(-) $ 1)) $ extractNRandom1M n n
            where n = length xs

-- Problem 26 Generate the combinations of K distinct objects chosen from the N 
-- elements of a list 



--combK :: (Ord a, Eq a) => Int -> [a] -> [[a]]
--combK 0 _ = [[]]
--combK _ [] = [[]]
--combK k (y:ys) = nub $ sort --  [(y:xs) | xs <- combK (k-1) ys , length(y:xs) == k]

        
-- Problem 28a Sort a list of lists according to the lenght of each sublist

lsort :: [[a]] -> [[a]]
lsort a = map snd $
          sortBy (\(x,_) (y,_) -> compare x y) $
          zip (map length a) a 
          
-- Problem 28b Sort a list of lists by reverse length frequency

lenCount :: Int -> [[a]] -> Int
lenCount _ [] = 0
lenCount n (x:xs)
  | n == length x = 1 + lenCount n xs
  | n /= length x = 0

-- -- under work, currently broken
-- lfsort :: [[a]] -> [[a]]
-- lfsort xs = concatMap snd $
--             sortBy (\ (x,_) (y,_) -> compare x y) $
--             groupBy (\ (x,_) (y,_) x==y) $
--             zip (map length ys) ys
--               where ys = lsort xs
--                     -- lengthPack :: [[a]] -> [(Int, [[a]])]
--                     -- lengthPack x = [(length x, [x])]
--                     -- lengthPack (x:xs) = (lenCount (length x) xs, take (lenCount(length x) xs) xs): lengthPack (drop (lenCount(length x) xs) (x:xs))
                                      