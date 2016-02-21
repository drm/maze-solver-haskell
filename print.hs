import Codec.Picture

printable 1 = "#"
printable 0 = " "
printable x = show x

p grid = 
    case grid of
        Nothing -> ""
        Just g -> unlines $ map (\row -> foldl (++) "" $ map printable row) g

imageToMaze :: DynamicImage -> Maybe [[Int]]
imageToMaze (ImageRGB8 image@(Image w h _)) = 
--    Just $ map (\y -> [y]) [0..h]
    Just ( map (
        \y -> 
            map ( 
                \(PixelRGB8 r _ _) -> 
                if r == 0 
                    then 1 
                    else 0
            )
            [pixelAt image x y | x <- [0..w-1]]
        )
        [0..h-1]
    )

imageToMaze _ = Nothing

main = do
    f <- readGif "./maze.gif"
    case f of
        Right i' -> putStrLn $ p $ imageToMaze i'
