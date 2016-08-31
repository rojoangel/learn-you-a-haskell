--
-- guards
--

max' :: (Ord a) => a -> a -> a
max' a b
    | a <= b    = b
    | otherwise = a

compare' :: (Ord a) => a -> a -> Ordering
compare' a b
    | a == b    = EQ
    | a <= b    = LT
    | otherwise = GT

bmiTell :: Double -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight, eat more!"
    | bmi <= 25.0 = "Looking good!"
    | bmi <= 30.0 = "You're overweight. Let's work out together!"
    | otherwise   = "You're obese. Go see a doctor."

bmiTell' :: Double -> Double -> String
bmiTell' weight height
    | weight / height ^ 2 <= 18.5 = "You're underweight, eat more!"
    | weight / height ^ 2 <= 25.0 = "Looking good!"
    | weight / height ^ 2  <= 30.0 = "You're overweight. Let's work out together!"
    | otherwise   = "You're obese. Go see a doctor."

--
-- where
--

bmiTell'' :: Double -> Double -> String
bmiTell'' weight height
    | bmi <= 18.5 = "You're underweight, eat more!"
    | bmi <= 25.0 = "Looking good!"
    | bmi <= 30.0 = "You're overweight. Let's work out together!"
    | otherwise   = "You're obese. Go see a doctor."
    where bmi = weight / height ^ 2

bmiTell''' :: Double -> Double -> String
bmiTell''' weight height
    | bmi <= skinny = "You're underweight, eat more!"
    | bmi <= normal = "Looking good!"
    | bmi <= fat    = "You're overweight. Let's work out together!"
    | otherwise     = "You're obese. Go see a doctor."
    where bmi    = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat    = 30.0

--
-- pattern matching with where
--

bmiTell'''' :: Double -> Double -> String
bmiTell'''' weight height
    | bmi <= skinny = "You're underweight, eat more!"
    | bmi <= normal = "Looking good!"
    | bmi <= fat    = "You're overweight. Let's work out together!"
    | otherwise     = "You're obese. Go see a doctor."
    where bmi                   = weight / height ^ 2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

--
-- functions in where blocks
--

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

--
-- let
--

cylinder :: Double -> Double -> Double
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea

--
-- let in list comprenhesions
--

calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [ bmi | (w, h) <- xs, let bmi = w / h ^ 2]

calcBmis'' :: [(Double, Double)] -> [Double]
calcBmis'' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi > 25.0]

--
-- case expressions
--

describeList :: [a] -> String
describeList ls = "The list is " ++ case ls of []  -> "empty."
                                               [x] -> "a singleton list."
                                               xs  -> "a longer list."

describeList' :: [a] -> String
describeList' ls = "The list is " ++ what ls
    where what []  = "empty."
          what [x] = "a singleton list."
          what xs  = "a longer list."

--
-- recursion
--

maximum' :: (Ord a) => [a] -> a
maximum' []     = error "maximum of empty list!"
maximum' [x]    = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x : replicate (n - 1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0  = []
take' _ []    = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' []    = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _          = []
zip' _ []          = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ []      = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = elem' a xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerOrEqual = [a | a <- xs, a <= x]
        greater        = [a | a <- xs, a > x]
    in  quicksort smallerOrEqual ++ [x] ++ quicksort greater

quicksort' :: (Ord a) => [a] -> [a]
quicksort' []    = []
quicksort' (x:xs) = quicksort' smallerOrEqual ++ [x] ++ quicksort' greater
    where smallerOrEqual = [a | a <- xs, a <= x]
          greater        = [a | a <- xs, a > x]

--
-- higher order & curried functions
--
compareWithHundred :: Int -> Ordering
compareWithHundred x = compare 100 x

compareWithHundred' :: Int -> Ordering
compareWithHundred' = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _          = []
zipWith' _ _ []          = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x

flip'' :: (a -> b -> c) -> (b -> a -> c)
flip'' f y x = f x y

-- map
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

-- using list comprenhension, instead
map'' :: (a -> b) -> [a] -> [b]
map'' f xs = [f x | x <- xs]

-- filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
    | f x       = x : filter' f xs
    | otherwise = filter' f xs

-- using list comprenhension, instead
filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f xs = [x | x <- xs, f x]

-- quicksort using filter
quicksort'' :: (Ord a) => [a] -> [a]
quicksort'' [] = []
quicksort'' (x:xs) = quicksort'' smallerOrEqual ++ [x] ++ quicksort'' greater
    where smallerOrEqual = filter (<= x) xs
          greater = filter (> x) xs

-- largest number under 100,000 that's divisible by 3,829
largestDivisible :: Integer
largestDivisible = head (filter divisible [100000,99999..])
    where divisible x = x `mod` 3829 == 0

-- sum of all odd squares that are less than 10,000
oddSquaresSum :: Integer
oddSquaresSum = sum (takeWhile (< 10000) (filter odd (map (^2) [1..])))

-- using list comprenhension, instead
oddSquaresSum' :: Integer
oddSquaresSum' = sum (takeWhile (< 10000)[ y |y <- [x^2 | x <- [1..]], odd y])

-- collatz chain
chain :: Integer -> [Integer]
chain 1 = [1]
chain n
    | even n = n : chain (n `div` 2)
    | odd n  = n : chain (3 * n + 1)

-- numLongChains
numLongChains :: Int
numLongChains = length (filter longerThan15 (map chain [1..100]))
    where longerThan15 xs = length xs > 15

--
-- lambdas
--
numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

addThree :: Int -> Int -> Int -> Int
addThree = \x -> \y -> \z -> x + y + z
-- addThree x y z = x + y + z

flip''' :: (a -> b -> c) -> (b -> a -> c)
flip''' f = \x y -> f y x

--
-- folds
--
sum'' :: (Num a) => [a] -> a
sum'' xs = foldl (\acc x -> acc + x) 0 xs

-- taking advantage of currying
sum''' :: (Num a) => [a] -> a
sum''' = foldl (+) 0
-- sum''' xs = foldl (+) 0 xs

-- map using foldr
map''' :: (a -> b) -> [a] -> [b]
map''' f xs = foldr (\x acc -> f x : acc) [] xs

-- map using foldl
map'''' :: (a -> b) -> [a] -> [b]
map'''' f xs = foldl (\acc x -> acc ++ [f x] ) [] xs

-- elem using foldr
elem'' :: (Eq a) => a -> [a] -> Bool
elem'' y ys = foldr (\x acc -> if x == y then True else acc) False ys
