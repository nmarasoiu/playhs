main = do
    fibosCnt <- getLine
    fibosCmds <- (sequence $ take (read fibosCnt) $ repeat getLine)
    _ <- sequence [putStrLn $ show $ fibF $ read n| n<-fibosCmds]
    return ()
fibonac :: [Integer]
fibonac = 0:1:[x+y|(x,y)<-zip fibonac $ tail fibonac]
fibF :: Int -> Integer
fibF n = (head $ drop n fibonac) `mod` (10^8+7)
