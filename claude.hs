import Data.List (intercalate, nub)
import Data.Array (Array, array, bounds, elems, indices, (!), (//))
import Data.Char (toLower)

type Board = Array (Int, Int) Piece
data Piece = Empty | RedKitten | RedCat | BlueKitten | BlueCat deriving (Eq, Show)
type Player = (Piece, Int, Int) -- (color, kittens, cats)

initialBoard :: Board
initialBoard = array ((0, 0), (5, 5)) [((x, y), Empty) | x <- [0..5], y <- [0..5]]

initialPlayers :: (Player, Player)
initialPlayers = ((RedKitten, 8, 0), (BlueKitten, 8, 0))

showPiece :: Piece -> Char
showPiece Empty = '.'
showPiece RedKitten = 'r'
showPiece RedCat = 'R'
showPitten BlueKitten = 'b'
showPitten BlueCat = 'B'

displayBoard :: Board -> String
displayBoard board = unlines [intercalate " " [showPiece (board ! (x, y)) | y <- [0..5]] | x <- [0..5]]

getPossibleMoves :: Player -> [((Int, Int), Piece)]
getPossibleMoves (color, kittens, cats)
    | kittens > 0 = [((x, y), color) | x <- [0..5], y <- [0..5], board ! (x, y) == Empty]
    | cats > 0 = [((x, y), NextPiece color) | x <- [0..5], y <- [0..5], board ! (x, y) == Empty]
    | otherwise = []
    where
        board = initialBoard
        NextPiece RedKitten = RedCat
        NextPiece BlueKitten = BlueCat
        NextPiece _ = error "Invalid player color"

parseMove :: String -> Maybe ((Int, Int), Piece)
parseMove (p:row:col:_)
    | isValidMove (row, col) (toLower p) = Just ((read [row] - 1, read [col] - 1), piece)
    | otherwise = Nothing
    where
        piece = case toLower p of
            'k' -> playerColor
            'c' -> case playerColor of
                     RedKitten -> RedCat
                     BlueKitten -> BlueCat
                     _ -> error "Invalid player color"
            _ -> error "Invalid move"
        isValidMove (row, col) p =
            row >= '1' && row <= '6' &&
            col >= '1' && col <= '6' &&
            (p == 'k' || p == 'c')
        playerColor = color $ fst initialPlayers

makeMove :: Board -> Player -> ((Int, Int), Piece) -> (Board, Player)
makeMove board player@(color, kittens, cats) ((row, col), piece) =
    (newBoard, (color, newKittens, newCats))
    where
        newBoard = foldl' pushPiece (board // [((row, col), piece)]) pushDirs
        pushDirs = getPushDirections board (row, col)
        getPushDirections b (x, y) = nub $ filter (\(dx, dy) -> isValidPush b (x, y) (dx, dy)) dirs
        dirs = [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)]
        isValidPush b (x, y) (dx, dy) =
            let (x', y') = (x + dx, y + dy)
                (loX, loY, hiX, hiY) = bounds b
            in x' >= loX && x' <= hiX && y' >= loY && y' <= hiY &&
               (b ! (x', y') == Empty || not (inBounds b (x' + dx, y' + dy)))
        pushPiece b (x, y) =
            if b ! (x, y) /= piece
                then b // [((x, y), b ! (x - dx, y - dy))]
                else b
            where
                (dx, dy) = getDirToNewPiece b (x, y) (row, col)
        getDirToNewPiece b (x, y) (r, c) =
            head [(dx, dy) | (dx, dy) <- dirs, x + dx == r, y + dy == c]
        newKittens = if piece == color then kittens - 1 else kittens
        newCats = if piece == NextPiece color then cats + 1 else cats
        NextPiece RedKitten = RedCat
        NextPiece BlueKitten = BlueCat
        NextPiece _ = error "Invalid player color"

checkWin :: Board -> Player -> Bool
checkWin board (color, _, _) =
    hasThreeInARow board color || hasTotalEight board color
    where
        hasThreeInARow b c =
            any (\[(x, y), (x', y'), (x'', y'')] ->
                     b ! (x, y) == b ! (x', y') && b ! (x', y') == b ! (x'', y'') && b ! (x, y) == c)
                triplets
        hasTotalEight b c = length (filter (== c) (elems b)) == 8
        triplets = [([(x, y), (x + dx, y + dy), (x + 2 * dx, y + 2 * dy)] |
                     x <- [0..3], y <- [0..5], (dx, dy) <- [(0, 1), (1, 0), (1, 1), (1, -1)],
                     inBounds b (x, y) && inBounds b (x + dx, y + dy) && inBounds b (x + 2 * dx, y + 2 * dy))]
        inBounds b (x, y) =
            let (loX, loY, hiX, hiY) = bounds b
            in x >= loX && x <= hiX && y >= loY && y <= hiY

checkUpgrade :: Board -> Player -> (Board, Player)
checkUpgrade board player@(color, kittens, cats) =
    case threeInARows of
        [] -> if hasTotalEight board color
                  then (newBoard, (color, kittens - 1, cats + 1))
                  else (board, player)
        (coords:_) -> (newBoard, (color, kittens - 3, cats + 3))
    where
        threeInARows =
            [(x, y, x + dx, y + dy, x + 2 * dx, y + 2 * dy) |
             x <- [0..3], y <- [0..5], (dx, dy) <- [(0, 1), (1, 0), (1, 1), (1, -1)],
             inBounds board (x, y) && inBounds board (x + dx, y + dy) && inBounds board (x + 2 * dx, y + 2 * dy) &&
             board ! (x, y) == color && board ! (x + dx, y + dy) == color && board ! (x + 2 * dx, y + 2 * dy) == color]
        newBoard = foldl' (\b (x, y) -> b // [((x, y), Empty)]) board coords
        coords = (x, y, x', y', x'', y'')
        (x, y) = head threeInARows
        (x', y') = threeInARows !! 1
        (x'', y'') = threeInARows !! 2
        hasTotalEight b c = length (filter (== c) (elems b)) == 8
        inBounds b (x, y) =
            let (loX, loY, hiX, hiY) = bounds b
            in x >= loX && x <= hiX && y >= loY && y <= hiY

play :: Board -> (Player, Player) -> IO ()
play board (redPlayer, bluePlayer) = do
    putStrLn $ "Red player: " ++ show redPlayer
    putStrLn $ "Blue player: " ++ show bluePlayer
    putStrLn $ displayBoard board
    if checkWin board redPlayer
        then putStrLn "Red player wins!"
        else if checkWin board bluePlayer
            then putStrLn "Blue player wins!"
            else do
                putStrLn "Red player's turn. Enter move (k/c row col):"
                move <- getLine
                case parseMove move of
                    Just m -> do
                        let (newBoard, newRedPlayer) = makeMove board redPlayer m
                        let (upgradedBoard, upgradedRedPlayer) = checkUpgrade newBoard newRedPlayer
                        play upgradedBoard (upgradedRedPlayer, bluePlayer)
                    Nothing -> do
                        putStrLn "Invalid move, try again."
                        play board (redPlayer, bluePlayer)

main :: IO ()
main = play initialBoard initialPlayers