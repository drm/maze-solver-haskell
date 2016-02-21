
f :: Int -> IO Int
f d = do
    putStr "Foo: "
    putStrLn $ show d
    return (d + 1)

main = do
   a <- f 10 
   putStrLn $ show a
