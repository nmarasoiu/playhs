main = do
    feed $ \n -> sum [n^t/(fac (fromIntegral t)) | t<-[0..9]] where fac t=foldl (*) 1 [1..t]

feed :: (Read a,Show b)=>(a->b) -> IO ()
feed f = do
    args<-getInput
    writeOutput $ map f args
    return ()

getInput::Read a=>IO [a]
getInput = do
    inputCount <- readLn
    sequence $ take inputCount $ repeat $ readLn

writeOutput :: Show b=>[b]->IO ()
writeOutput bs = do
    sequence [putStrLn $ show b|b<-bs]
    return ()
