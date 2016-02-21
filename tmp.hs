import Codec.Picture (readGif)

printable 1 = "#"
printable 0 = " "

p grid = unlines $ map (\row -> foldl (++) "" $ map printable row) grid

main = do
    putStrLn $  p [[0, 1, 1, 0, 1], [1, 0, 0, 0, 0]]
