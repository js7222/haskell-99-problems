
import Data.List

data NestedList a = Elem a | List [NestedList a]

data Encoded a = Multiple Int a | Single a deriving Show

flatten :: NestedList a -> [a]
flatten nl = innerFlatten nl []
    where innerFlatten (Elem x) xs = x:xs
          innerFlatten (List ys) xs = foldr innerFlatten xs ys

compress :: Eq a => [a] -> [a]
compress xs = map head $ Data.List.group xs

encode :: Eq a => [a] -> [(Int, a)]
encode xs = map pack $ Data.List.group xs
    where pack xs = (length xs, head xs)

encodeModified :: Eq a => [a] -> [Encoded a]
encodeModified xs = map pack' $ Data.List.group xs

encodeModified' :: Eq a => [a] -> [Encoded a]
encodeModified' xs = map encodeModifiedHelper' $ encode xs
    where encodeModifiedHelper' (1,x) = Single x
          encodeModifiedHelper' (n,x) = Multiple n x

decodeModified :: Eq a =>  [Encoded a] -> [a]
decodeModified xs = concatMap decodeEncoded xs
    where decodeEncoded (Multiple n x) = replicate n x
          decodeEncoded (Single x) = [x]


pack' :: [a] -> Encoded a
pack' xs
    | length xs == 1 = Single $ head xs
    | otherwise = Multiple (length xs)  $ head xs

dupli :: [a] -> [a]
dupli xs = concatMap (replicate 2) xs
