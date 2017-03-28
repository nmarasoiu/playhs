main = do
    line1 <- getLine
    line2 <- getLine
    sequence [putStrLn resLine| resLine<-prefixEncode (line1:[line2])]
    return ()

prefixEncode::[String]->[String]
prefixEncode lines@(line1:line2:[]) = map decor $ prefix:suffixes
    where
        decor :: String -> String
        decor str = len++(' ': str) where len = show $ length str
        prefix :: String
        prefix = [letter | (letter,l2) <- takeWhile (\(l1,l2)->l1==l2) $ zip line1 line2]
        suffixes :: [String]
        suffixes = map (drop (length prefix)) lines