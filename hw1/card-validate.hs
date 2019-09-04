import Data.List

toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0 = []
  | otherwise = map read . words . intersperse ' ' $ show x


toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse $ go 1 (reverse xs)
  where go _ [] = []
        go i (y:ys)
          | even i = (2*y) : (go (i+1) ys)
          | otherwise = y : (go (i+1) ys)

sumDigits :: [Integer] -> Integer
sumDigits = sum . concat . map toDigits

validate :: Integer -> Bool
validate = (==0) . (flip rem) 10 . sumDigits . doubleEveryOther . toDigits
