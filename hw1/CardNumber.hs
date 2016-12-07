toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0     = []
    | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

doubleEveryOther :: Integer -> [Integer]
doubleEveryOther = zipWith (*) (cycle [1, 2]) . toDigitsRev

sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigitsRev)

validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther

main :: IO ()
main = do
    print $ toDigits 12345
    print $ toDigitsRev 12345
    print $ doubleEveryOther 8765
    print $ doubleEveryOther 123
    print $ sumDigits [16, 7, 12, 5]
    print $ validate 4012888888881881
    print $ validate 4012888888881882

