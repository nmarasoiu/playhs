main = do
    bunniesCount <- readLn::IO Int
    bunniesJumpLengths <- getLine
--     putStrLn $ show $ split bunniesJumpLengths
    putStrLn $ show $ cmmmc $ map read $ split bunniesJumpLengths
    return ()

cmmmc::[Int]->Int
cmmmc ints = foldl (\acc n -> (acc*n) `div` (cmmdc (acc:[n]))) 1 ints
    where
        prod::[Int]->Int
        prod ints = foldl (*) 1 ints
        cmmdc::[Int]->Int
        cmmdc ints@(i1:i2:i3:iis) = foldl accF 1 ints
            where
                accF::(Int->Int->Int)
                accF cmmdcSoFar number = cmmdc [cmmdcSoFar, number]
        cmmdc (i1:[i2]) | i1==i2 = i1
        cmmdc (i1:[i2]) | i1>i2 = cmmdc $ (i1-i2):[i2]
        cmmdc (i1:[i2]) | i1<i2 = cmmdc $ (i2:[i1])
        cmmdc [] = 1
        cmmdc [i] = i

split :: String -> [String]
split str@(l:ls) = s1:if null s2 then [] else (split $ tail s2) where (s1,s2)=span (/=' ') str
split [] = []
