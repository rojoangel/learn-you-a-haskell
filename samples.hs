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
