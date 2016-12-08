type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
    | n < 1     = error "Hanoi puzzle needs at least one disc."
    | n == 1    = [(a, b)]
    | otherwise = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a

solve :: Integer -> [Move]
solve n = hanoi n "a" "b" "c"

main :: IO ()
main = do
    print $ solve 2
    print $ solve 3
