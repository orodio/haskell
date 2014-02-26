rev :: [a] -> [a]
rev []     = []
rev (x:xs) = rev xs ++ [x]

integerToList :: Integer -> [Integer]
integerToList ns = [castInteger x | x <- show ns]
  where castInteger y = read [y] :: Integer

doubleAndSum :: Integer -> Integer
doubleAndSum = sum . integerToList . (*2)

doubleAndSumSecond :: [Integer] -> [Integer]
doubleAndSumSecond []       = []
doubleAndSumSecond [x]      = [x]
doubleAndSumSecond (x:y:zs) = x : doubleAndSum y : doubleAndSumSecond zs

luhn :: Integer -> Bool
luhn = (== 0) . (`mod` 10) . sum . doubleAndSumSecond . rev . integerToList
