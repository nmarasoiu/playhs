main = do
    count0 <- readLn :: IO Int
    answers <- iter count0 funcTst
    sequence [putStrLn answer | answer<-answers]
    return ()
iter::Int->IO a->IO [a]
iter count0 action = sequence $ take count0 $ repeat action
funcTst :: IO String
funcTst = do
    count <- readLn :: IO Int
    domain <- iter count readFirstElem
    return $ if isOk domain then "YES" else "NO"
isOk::[String]->Bool
isOk [] = True
isOk (d1:domain) = a&&b where a = 0==length (filter (==d1) domain); b = isOk domain
readFirstElem :: IO String
readFirstElem = do
    line <- getLine
    return $ takeWhile (\ char -> char/=' ') line
