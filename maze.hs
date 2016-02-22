import Codec.Picture

data Tile 
    = Wall 
    | Path 
    deriving (Eq, Show)

data Dir 
    = North 
    | East 
    | South 
    | West 
    deriving Show

data Rot 
    = CW            -- Rotate clockwise
    | CCW           -- Rotate counter clockwise
    | Turn          -- Turn around
    | None          -- Don't rotate

data Pos 
    = Pos Int Int           -- A position is composed of two Int coordinates
    deriving (Eq, Show)

data State 
    = State Pos Dir 
    deriving Show

data Maze               
    = Maze [[Tile]]     -- A maze consists of rows of columns of Tiles (i.e. two-dimensional list)

-- Render a printable version of the Tile type
printable :: Tile -> String
printable Wall = "#"
printable Path = " "

-- Get the type of tile at the given position from the given Maze. Is not index-safe
at :: Maze -> Pos -> Tile
at (Maze maze) (Pos x y) = maze !! x !! y

-- Test if the given position within the maze is the edge of the entire maze
isEdge :: Maze -> Pos -> Bool
isEdge (Maze maze) (Pos x y) = x == 0 || y == 0 || x == length maze - 1 || y == length (maze !! x) -1 

-- Test if the given position is existent within the maze
valid :: Maze -> Pos -> Bool
valid (Maze maze) (Pos x y) = x < (length maze) && y < (length $ maze !! x)

-- Translate a position one step in the given direction
translate :: Pos -> Dir -> Pos
translate (Pos x y) West = Pos (x - 1) y
translate (Pos x y) East = Pos (x + 1) y
translate (Pos x y) North = Pos x (y - 1)
translate (Pos x y) South = Pos x (y + 1)

-- Rotate a direction by a given rotation
rotate :: Dir -> Rot -> Dir
rotate North CW = East
rotate East CW = South
rotate South CW = West
rotate West CW = North
rotate d CCW = rotate (rotate (rotate d CW) CW) CW
rotate d Turn = rotate (rotate d CW) CW
rotate d None = d

-- Returns the preferred list of rotations. This does a "Wall follower" algorithm
-- by preferring to rotate right (CW), then proceeding ahead (None), then left (CCW), 
-- and ultimately turn back
rotations :: Dir -> [Dir]
rotations d = map (rotate d) [CW, None, CCW, Turn]

-- Find the next position given the current maze, position and looking direction, iterating over the passed 
-- directions, yielding a new state or Nothing if no move can be calculated
nextPos :: Maze -> Pos -> Dir -> [Dir] -> Maybe State 
nextPos maze pos dir [] = Nothing
nextPos maze pos dir l = 
    let t = head l
        n = translate pos t in
        if (valid maze n) && (at maze n == Path)
            then Just (State n t)
            else nextPos maze pos dir $ tail l

-- Walk the maze by 
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
