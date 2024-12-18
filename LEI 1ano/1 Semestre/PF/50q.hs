-- 1 enumFromTo
enumFromTo' :: Int -> Int -> [Int]
enumFromTo' a b 
    |   a < b = a : enumFromTo' (a+1) b
    |   a == b = [b]
    |   otherwise = b : enumFromTo' (b+1) a

-- 2 enumFromThenTo
enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' _ 0 _ = []
enumFromThenTo' s n e 
    |   s > e = []
    |   otherwise = s : enumFromThenTo' n (2*n-s) e

-- 3 (++)
juncao :: [a] -> [a] -> [a]
juncao [] [] = []
juncao l [] = l
juncao [] l = l
juncao (x:xs) l = x : juncao xs l

-- 4 (!!)
position :: [a] -> Int -> a 
position (x:xs) 0 = x
position (x:xs) n = position xs (n-1)

-- 5 reverse
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse xs ++ [x]

-- 6 take
take' :: Int -> [a] -> [a]
take _ [] = []
take' 0 (x:xs) = xs
take' n (x:xs) = x : take' (n-1) (xs) 

-- 7 drop 
drop' :: Int -> [a] -> [a]
drop' 0 l = l
drop' 1 (x:xs) = xs
drop' n (x:xs) = drop' (n-1) (xs)

-- 8 zip
zip' :: [a] -> [b] -> [(a,b)]
zip' [] [] = []
zip' [] l = []
zip' l [] = []
zip' (x:xs) (y:ys) = [(x,y)] ++ zip' xs ys

-- 9 replicate
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' 1 x = [x]
replicate' n x = x : replicate' (n-1) x

-- 10 intersperse
intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse n (x:xs) = x : n : intersperse n xs

-- 11 group
group :: Eq a => [a] -> [[a]]
group [] = []
group [x] = [[x]]
group (x:xs) 
    |   elem x (head g) = (x : (head g)) : tail g
    |   otherwise = [x] : g
        where g = group xs

-- 12 concat
concat' :: [[a]] -> [a]
concat' [] = []
concat' [l] = l
concat' (x:t)= x ++ concat' t

-- 13 inits
inits :: [a] -> [[a]]
inits [] = []
inits [a] = [[a]]
inits l = (inits (init l)) ++ [l]

-- drop 
drop2 :: Int -> [a] -> [a]
drop2 _ [] = []
drop2 0 l = l
drop2 n (x:xs) = drop2 (n-1) xs