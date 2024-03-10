This program defines a Board class that represents the game board and implements the rules of the game. The play_game function sets up a new board and prompts the players for their moves until one of them wins.

Here's how the program works:

1. The Board class initializes an empty 6x6 grid, keeps track of each player's supply of pieces, and keeps track of whose turn it is.

1. The place_piece method allows a player to place a piece on the board. It checks if the move is valid, pushes adjacent pieces as needed, checks for a win condition (three in a row or eight on the board), upgrades kittens to cats if possible, and switches to the other player's turn.

1. The push_adjacent method pushes any adjacent pieces away from the newly placed piece, moving them off the board if necessary.

1. The push_piece method moves a piece in a given direction until it hits the edge of the board or an occupied square.

1. The has_three_in_row method checks if a player has three pieces of a given type (kitten or cat) in a row, horizontally, vertically, or diagonally.

1. The has_eight_on_board method checks if a player has eight pieces on the board.

1. The upgrade_three_kittens method replaces three kittens in a row with three cats in the player's supply.

1. The upgrade_single_kitten method replaces a single kitten on the board with a cat from the player's supply.

1. The print_board method displays the current state of the board and the players' supplies.

The play_game function sets up a new board and prompts the players to enter their moves until one of them wins.
Note that this program assumes valid input from the players and does not handle exceptions or invalid moves.