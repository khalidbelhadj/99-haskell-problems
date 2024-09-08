module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"

-- 1
myLast :: [a] -> a
myLast = head . reverse

-- 2
myButLast :: [a] -> a
myButLast = head . tail . reverse

-- 3
elementAt :: [a] -> Int -> a
elementAt [] _ = error "empty list"
elementAt xs 0 = head xs
elementAt (x:xs) n = elementAt xs (n - 1)

-- 4
myLength :: [a] -> Int
myLength = foldr (\ x -> (+) 1) 0

-- 5
myReverse :: [a] -> [a]
dsljk
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- 6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == myReverse xs

-- 7
data NestedList a = Elem a | List [NestedList a]
newtype S = String Int
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = concatMap flatten xs

-- 8
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress xs = map fst diffPairs ++ [snd $ myLast diffPairs]
  where diffPairs = filter (uncurry (/=)) (zip xs (tail xs))

compress' :: (Eq a) => [a] -> [a]
compress' [] = []
compress' (x:xs) = x : compress' (dropWhile (== x) xs)

-- 9
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)

-- 10
encode :: (Eq a) => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . pack

