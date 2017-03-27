main = do
    input <- getLine
    putStrLn $ compress [] input
    return ()
compress :: String->String -> String
compress pref [] = reverse pref
compress oldPrefix oldString = compress newPrefix newString
    where
        currentChar = head oldString
        newPrefix = currentChar:oldPrefix
        newString = dropWhile charAlreadyInNewPrefix oldString
        charAlreadyInNewPrefix char = not $ null $ filter (==char) newPrefix

-- compress str = foldl1 accF (Char,String) str
--     where
--         accF::(Char,String)->Char->(Char,String)
--         accF (accCh,accStr) crtCh = (crtCh,accCh++[crtCh])
--
--         (ch, n, str2) = foldl accF ('d',0,"") str
--         res = append str2 ch n
--         accF :: (Char, Int, String) -> Char -> (Char, Int, String)
--         accF (ch0, 0, str) ch = (ch, 1, str)
--         accF (ch1, n, str) ch2 | (ch1==ch2) && (n>0) = (ch1, n+1, str)
--         accF (ch1, n, str) ch2 | (ch1/=ch2) && (n>0) = (ch2, 1, append str ch1 n)
--         append :: String -> Char -> Int -> String
--         append str ch 1 = str ++ [ch]
--         append str ch n | n>1 = str ++ [ch] ++ (show n)