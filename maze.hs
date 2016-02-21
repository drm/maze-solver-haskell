import Codec.Picture

data T = Wall | Path deriving (Eq, Show)
data Dir = North | East | South | West deriving Show
data Rot = CW | CCW | Turn | None
data Pos = Pos Int Int deriving (Eq, Show)
data State = State Pos Dir deriving Show
data Maze = Maze [[T]]

printable Wall = "#"
printable Path = " "

at :: Maze -> Pos -> T
at (Maze maze) (Pos x y) = maze !! x !! y

isEdge (Maze maze) (Pos x y) = x == 0 || y == 0 || x == length maze - 1 || y == length (maze !! x) -1 

valid :: Maze -> Pos -> Bool
valid (Maze maze) (Pos x y) = x < (length maze) && y < (length $ maze !! x)

translate :: Pos -> Dir -> Pos
translate (Pos x y) West = Pos (x - 1) y
translate (Pos x y) East = Pos (x + 1) y
translate (Pos x y) North = Pos x (y - 1)
translate (Pos x y) South = Pos x (y + 1)

rotate :: Dir -> Rot -> Dir
rotate North CW = East
rotate East CW = South
rotate South CW = West
rotate West CW = North
rotate d CCW = rotate (rotate (rotate d CW) CW) CW
rotate d Turn = rotate (rotate d CW) CW
rotate d None = d

rotations :: Dir -> [Dir]
rotations d = map (rotate d) [CW, None, CCW, Turn]

nextPos :: Maze -> Pos -> Dir -> [Dir] -> Maybe State 
nextPos maze pos dir [] = Nothing
nextPos maze pos dir l = 
    let t = head l
        n = translate pos t in
        if (valid maze n) && (at maze n == Path)
            then Just (State n t)
            else nextPos maze pos dir $ tail l

walk :: Maze -> Pos -> Dir -> [Pos] -> IO()
walk maze pos dir visited = 
    let n' = nextPos maze pos dir (rotations dir) in
        case n' of 
            Just (State pos' dir') -> do
                if pos `elem` visited && isEdge maze pos
                    then 
                        putStrLn $ showMaze maze visited 
                    else do 
                        putStrLn $ "next position: " ++ (show pos')
                        walk maze pos' dir' (pos:visited)
            Nothing -> 
                putStrLn "Unsolvable :("
    
showMaze :: Maze -> [Pos] -> String
showMaze (Maze grid) visited = unlines 
    $ map (
        \x -> foldl (++) "" 
        $ map (
            \y -> if Pos x y  `elem` visited 
                then if isEdge (Maze grid) (Pos x y) then " " else "."
                else printable $ at (Maze grid) (Pos x y)
        ) [0..(length $ grid !! x) - 1]
    ) [0..(length grid) - 1]

imageToMaze :: DynamicImage -> Maybe Maze
imageToMaze (ImageRGB8 image@(Image w h _)) = 
    Just $ Maze ( map (
        \y -> 
            map ( 
                \(PixelRGB8 r _ _) -> 
                if r == 0 
                    then Wall
                    else Path
            )
            [pixelAt image x y | x <- [0..w-1]]
        )
        [0..h-1]
    )

imageToMaze _ = Nothing

main = do
    f <- readGif "./maze.gif"
    case f of
        Left _ -> error "Could not read file" 
        Right i' -> 
            let m = imageToMaze i' in
                case m of
                    Nothing -> error "Could not convert to maze"
                    Just maze -> walk maze (Pos 1 0) East []
