data Foo = Foo Int Int
data Bar = Bar Int Int

m :: Foo -> Int -> Int

m (Foo x y) 0 = 1
m (Foo x y) 2 = 100

n x y = Bar x y

main = do
    putStrLn $ show $ m (n 1 2) 2
