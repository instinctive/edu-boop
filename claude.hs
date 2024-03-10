import Data.List (intercalate, nub)
import Data.Array (Array, array, bounds, elems, indices, (!), (//))
import Data.Maybe (isJust, fromJust)

data Color = Red | Blue deriving (Eq, Show)
data PieceType = Kitten | Cat deriving (Eq, Show)
type Piece = Maybe (Color, PieceType)
type Board = Array (Int, Int) Piece
type Player = (Color, Int, Int) -- (color, kittens, cats)

initialBoard :: Board
initialBoard = array ((0, 0), (5, 5)) [((x, y), Nothing) | x <- [0..5], y <- [0..5]]

initialPlayers :: (Player, Player)
initialPlayers = ((Red, 8, 0), (Blue, 8, 0))

showPiece :: Piece -> Char
showPiece Nothing = '.'
showPiece (Just (Red, Kitten)) = 'r'
showPiece (Just (Red, Cat)) = 'R'
showPiece (Just (Blue, Kitten)) = 'b'
showPiece (Just (Blue, Cat)) = 'B'

displayBoard :: Board -> String
displayBoard board = unlines [intercalate " " [showPiece (board ! (x, y)) | y <- [0..5]] | x <- [0..5]]

getPossibleMoves :: Player -> [((Int, Int), Piece)]
getPossibleMoves (color, kittens, cats)
    | kittens > 0 = [((x, y), Just (color, Kitten)) | x <- [0..5], y <- [0..5], board ! (x, y) == Nothing]
    | cats > 0 = [((x, y), Just (color, Cat)) | x <- [0..5], y <- [0..5], board ! (x, y) == Nothing]
    | otherwise = []
    where
        board = initialBoard

parseMove :: String -> Maybe ((Int, Int), Piece)
parseMove (p:row:col:_)
    | isValidMove (row, col) (toLower p) = Just ((read [row] - 1, read [col] - 1), piece)
    | otherwise = Nothing
    where
        piece = case toLower p of
            'k' -> Just (playerColor, Kitten)
            'c' -> Just (playerColor, Cat)
            _ -> error "Invalid move"
        isValidMove (row, col) p =
            row >= '1' && row <= '6' &&
            col >= '1' && col <= '6' &&
            (p == 'k' || p == 'c')
        playerColor = fst initialPlayers

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
               (b ! (x', y') == Nothing || not (inBounds b (x' + dx, y' + dy)))
        pushPiece b (x, y) =
            if isJust (b ! (x, y)) && b ! (x, y) /= piece
                then b // [((x, y), b ! (x - dx, y - dy))]
                else b
            where
                (dx, dy) = getDirToNewPiece b (x, y) (row, col)
        getDirToNewPiece b (x, y) (r, c) =
            head [(dx, dy) | (dx, dy) <- dirs, x + dx == r, y + dy == c]
        newKittens = if isJust piece && fromJust piece == (color, Kitten) then kittens - 1 else kittens
        newCats = if isJust piece && fromJust piece == (color, Cat) then cats + 1 else cats

checkWin :: Board -> Player -> Bool
checkWin board (color, _, _) =
    hasThreeInARow board color || hasTotalEight board color
    where
        hasThreeInARow b c =
            any (\[(x, y), (x', y'), (x'', y'')] ->
                     isJust (b ! (x, y)) && isJust (b ! (x', y')) && isJust (b ! (x'', y'')) &&
                     fromJust (b ! (x, y)) == (c, p) && fromJust (b ! (x', y')) == (c, p) && fromJust (b ! (x'', y'')) == (c, p))
                triplets
        hasTotalEight b c = length (filter (\p -> isJust p && fromJust p == (c, Cat)) (elems b)) == 8
        triplets = [([(x, y), (x + dx, y + dy), (x + 2 * dx, y + 2 * dy)] |
                     x <- [0..3], y <- [0..5], (dx, dy) <- [(0, 1), (1, 0), (1, 1), (1, -1)],
                     inBounds b (x, y) && inBounds b (x + dx, y + dy) && inBounds b (x + 2 * dx, y + 2 * dy))]
        inBounds b (x, y) =
            let (loX, loY, hiX, hiY) = bounds b
            in x >= loX && x <= hiX && y >= loY && y <= hiY
        p = Cat -- or Kitten, doesn't matter for the win condition

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
             isJust (board ! (x, y)) && isJust (board ! (x + dx, y + dy)) && isJust (board ! (x + 2 * dx, y + 2 * dy)) &&
             fromJust (board ! (x, y)) == (color, Kitten) && fromJust (board ! (x + dx, y + dy)) == (color, Kitten) && fromJust (board ! (x + 2 * dx, y + 2 * dy)) == (color, Kitten)]
        newBoard = foldl' (\b (x, y) -> b // [((x, y), Nothing)]) board coords
        coords = (x, y, x', y', x'', y'')
        (x, y) = head threeInARows
        (x', y') = threeInARows !! 1
        (x'', y'') = threeInARows !! 2
        hasTotalEight b c = length (filter (\p -> isJust p && fromJust p == (c, Cat)) (elems b)) == 8
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