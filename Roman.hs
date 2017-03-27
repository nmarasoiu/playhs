tens = "IXCM"
fives = "VLD"

singleDigitConverter:: (Eq a, Ord a, Num a) => a -> a -> String
singleDigitConverter level n =
    if n < 4 then rep n one
    else if n == 4 then [one, five]
    else if n == 5 then [five]
    else if n  < 9 then five : rep (n-5) one
    else if n == 9 then [one, ten]
    else [ten]
        where
            rom = shift level tens
            one = head rom
            five = head $ tail rom
            ten = head $ tail $ tail rom

converter :: (Show a, Eq a, Ord a, Num a) => a -> String
converter a = foldMap trans (zip (show a) naturals)
    where
        trans :: (Read a, Eq a, Ord a, Num a) => (Char, a) -> String
        trans (digitChar, index) = singleDigitConverter index num
            where
                num :: (Read a, Eq a, Ord a, Num a) => a
                num = read [digitChar]

shift :: (Eq n, Num n) => n -> [a] -> [a]
shift n list =
    if n == 0 then list
    else tail $ shift (n-1) list

rep:: (Eq n, Num n) => n->Char -> String
rep n c = if n==0 then [] else c : rep (n-1) c

naturals :: (Eq a, Ord a, Num a) => [a]
naturals = 0 : map (+1) naturals