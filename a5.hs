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

all_basic_bit_seqs :: Int -> [[Int]]
all_basic_bit_seqs n = sequence (replicate n [0, 1])

data Bit = Zero | One
    deriving (Show, Eq)

flipBit :: Bit -> Bit
flipBit x | x == Zero = One
          | otherwise = Zero

invert :: [Bit] -> [Bit]
invert xs = [(flipBit x) | x <- xs]

all_bit_seqs :: Int -> [[Bit]]
all_bit_seqs n = sequence (replicate n [Zero, One])

bitVal1 :: Bit -> Int
bitVal1 x | x == Zero = 0
          | otherwise = 1

bitSum1 :: [Bit] -> Int
bitSum1 xs = sum (map bitVal1 xs)

bitVal2 :: Maybe Bit -> Int
bitVal2 x | x == Just One = 1
          | otherwise     = 0

bitSum2 :: [Maybe Bit] -> Int
bitSum2 xs = sum (map bitVal2 xs)

data List a = Empty | Cons a (List a)
    deriving Show

toList :: [a] -> List a
toList []     = Empty
toList (x:xs) = Cons x (toList xs)

toHaskellList :: List a -> [a]
toHaskellList Empty      = []
toHaskellList (Cons h r) = h : (toHaskellList r)

append :: List a -> List a -> List a
append Empty (Cons h r) = Cons h r
append (Cons h1 r1) (Cons h2 r2) = Cons h1 (append r1 (Cons h2 r2))

removeAll :: (a -> Bool) -> List a -> List a
removeAll f Empty                  = Empty
removeAll f (Cons h r) | f h       = removeAll f r
                       | otherwise = Cons h (removeAll f r)

sort :: Ord a => List a -> List a
sort Empty      = Empty
sort (Cons h r) = append smalls (Cons h bigs)
                	where 
                		smalls = sort (removeAll (\x -> x > h) r)
                		bigs   = sort (removeAll (\x -> x <= h) r)                    