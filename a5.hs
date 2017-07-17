snoc :: a -> [a] -> [a]
snoc a [] = [a]
snoc a (x:xs) = x : (snoc a xs)

myappend :: [a] -> [a] -> [a]
myappend [] ys     = ys
myappend (x:xs) ys = x : (myappend xs ys)

myreverse :: [a] -> [a]
myreverse []     = []
myreverse (x:xs) = myappend (myreverse xs) [x]

one_is_only_factor :: Int -> Int -> Bool
one_is_only_factor n s | s == 1           = True
                       | (n `mod` s) == 0 = False
                       | otherwise        = (one_is_only_factor n (s - 1))

is_prime :: Int -> Bool
is_prime n | n < 2     = False
           | otherwise = one_is_only_factor n (n - 1)

reversePosInt :: Int -> Int
reversePosInt n | n < 0     = error "reversePosInt: can only reverse non-negative integers"
                | n == 0    = 0
                | otherwise = (n `mod` 10) * (10 ^ i) + reversePosInt (n `div` 10)
                	where
                		n' = fromIntegral n
                		i  = floor (logBase 10 n')

is_emirp :: Int -> Bool
is_emirp n | n < 13     = False
           | otherwise  = (not (n == (reversePosInt n))) && (is_prime n) && (is_prime (reversePosInt n)) 

count_emirps :: Int -> Int
count_emirps n | n < 13      = 0
               | is_emirp n  = 1 + (count_emirps (n - 1))
               | otherwise   = count_emirps (n - 1)

greatest_item :: (a -> Int) -> [a] -> a -> a
greatest_item f xs max | null xs                 = max
                       | (f (head xs)) < (f max) = greatest_item f (tail xs) max
					   | otherwise               = greatest_item f (tail xs) (head xs)

biggest_sum :: [[Int]] -> [Int]
biggest_sum (x:xs) = greatest_item sum xs x

greatest :: (a -> Int) -> [a] -> a
greatest f (x:xs) = greatest_item f xs x

is_bit :: Int -> Bool
is_bit x | (x == 1) || (x == 0) = True
         | otherwise            = False

flip_bit :: Int -> Int
flip_bit x | not (is_bit x) = error "flip_bit: value is not 0 or 1"
           | x == 1         = 0
           | otherwise      = 1

is_bit_seq1 :: [Int] -> Bool
is_bit_seq1 xs | (null xs)              = True
               | not (is_bit (head xs)) = False
               | otherwise              = is_bit_seq1 (tail xs)

is_bit_seq2 :: [Int] -> Bool
is_bit_seq2 [] = True
is_bit_seq2 xs = if (not (is_bit (head xs))) then False else is_bit_seq2 (tail xs)

is_bit_seq3 :: [Int] -> Bool
is_bit_seq3 [] = True
is_bit_seq3 xs = all is_bit xs

invert_bits1 :: [Int] -> [Int]
invert_bits1 []     = []
invert_bits1 (x:xs) = (flip_bit x) : (invert_bits1 xs)

invert_bits2 :: [Int] -> [Int]
invert_bits2 xs = map flip_bit xs

invert_bits3 :: [Int] -> [Int]
invert_bits3 xs = [(flip_bit x) | x <- xs]

count_bits :: [Int] -> (Int, Int) -> (Int, Int)
count_bits xs t | null xs                = t
                | not (is_bit (head xs)) = error "bit_count: value is not 0 or 1"
                | (head xs) == 0         = count_bits (tail xs) ((fst t) + 1, snd t)
                | otherwise              = count_bits (tail xs) (fst t, (snd t) + 1)

bit_count :: [Int] -> (Int, Int)
bit_count xs = count_bits xs (0,0)

all_bit_seqs :: Int -> [[Int]]
all_bit_seqs n | n < 1     = []
               | n == 1    = [[0], [1]]
               | otherwise = add_bit (all_bit_seqs (n - 1))
               		where add_bit seqs = [b : seq | seq <- seqs, b <- [0,1]]