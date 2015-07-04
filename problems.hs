
import Data.List

data NestedList a = Elem a | List [NestedList a]

data Encoded a = Multiple Int a | Single a deriving Show

--problem 7
flatten :: NestedList a -> [a]
flatten nl = innerFlatten nl []
    where innerFlatten (Elem x) xs = x:xs
          innerFlatten (List ys) xs = foldr innerFlatten xs ys

--problem 8
compress :: Eq a => [a] -> [a]
compress xs = map head $ Data.List.group xs

--problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode xs = map pack $ Data.List.group xs
    where pack xs = (length xs, head xs)

--problem 11
encodeModified :: Eq a => [a] -> [Encoded a]
encodeModified xs = map pack' $ Data.List.group xs where
   pack' xs
       | length xs == 1 = Single $ head xs
       | otherwise = Multiple (length xs)  $ head xs

encodeModified' :: Eq a => [a] -> [Encoded a]
encodeModified' xs = map encodeModifiedHelper' $ encode xs
    where encodeModifiedHelper' (1,x) = Single x
          encodeModifiedHelper' (n,x) = Multiple n x

--problem 12
decodeModified :: Eq a =>  [Encoded a] -> [a]
decodeModified xs = concatMap decodeEncoded xs
    where decodeEncoded (Multiple n x) = replicate n x
          decodeEncoded (Single x) = [x]


--problem 14
dupli :: [a] -> [a]
dupli xs = concatMap (replicate 2) xs

--problem 15
repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

--problem 16
drop' :: [a] -> Int -> [a]
drop' [] _ = []
drop' (x:xs) 1 = drop' xs 0
drop' (x:xs) k = x:(drop' xs (k-1))
          

split' :: [a] -> Int -> ([a], [a])
split' xs n = splitHelper' xs [] n
    where splitHelper' xs ys 0 = (ys, xs)
          splitHelper' [] ys n = (ys, [])
          splitHelper' (x:xs) ys n = splitHelper' xs (ys ++ [x]) (n-1)

split'' :: [a] -> Int -> ([a], [a])
split'' xs n = ((take n xs), (drop n xs))
