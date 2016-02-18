data T = Wall | Path deriving Show
data Dir = North | East | South | West 
data Rot = CW | CCW | Turn | None
data Pos = Pos Int Int

maze :: [[T]]

maze =
    [[Path, Path, Path, Path, Path],
     [Path, Wall, Wall, Wall, Path],
     [Path, Path, Path, Wall, Path],
     [Path, Wall, Path, Path, Path],
     [Path, Wall, Wall, Wall, Path]]

at :: Pos -> T
at Pos x y = maze !! x !! y

valid :: Pos -> Bool
valid Pos x y = x < (length maze) && y < (length $ maze !! x)

move :: Pos -> Dir -> Pos
move (Pos x y) West = Pos (x - 1) y
move (Pos x y) East = Pos (x + 1) y
move (Pos x y) North = Pos x (y - 1)
move (Pos x y) South = Pos x (y + 1)

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

step :: [[T]] -> Pos -> Dir
step maze pos dir = do
    let n = move pos dir
    if valid n
        then walk maze n dir
        else error "Out of bounds"    

try_turns maze pos dir [] = do 
    error "Out of options"
try_turns maze pos dir l = do
    let n = move pos . head l
    if (valid n) && (at n == Path)
        then step maze pos $ head l
        else try_turns maze pos dir $ tail l

turn maze pos dir =
    try_turns maze pos dir . rotations dir

walk maze pos dir = 
    putStrLn pos
    step maze pos (turn maze pos dir)

main = do
    walk maze (Pos 1 0) East

--walk(Maze, Pos, Dir) ->
--    {X, Y} = Pos,
--    io:fwrite("Checking (~B, ~B) => ~B~n", [X, Y, at(Maze, Pos)]),
--    Dir = turn(Maze, Pos, Dir),
--    step(Maze, Pos, Dir).
--
---start() -> walk(maze(erl_img:load("./maze.gif"))).
