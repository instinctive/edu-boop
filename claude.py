import copy

class Board:
    def __init__(self):
        self.grid = [[None for _ in range(6)] for _ in range(6)]
        self.red_kittens = 8
        self.red_cats = 0
        self.blue_kittens = 8
        self.blue_cats = 0
        self.player_turn = 'red'

    def is_empty(self, row, col):
        return self.grid[row][col] is None

    def place_piece(self, move):
        piece_type, row, col = move.split()
        color = self.player_turn
        if piece_type == 'k':
            if color == 'red' and self.red_kittens == 0:
                return False
            if color == 'blue' and self.blue_kittens == 0:
                return False
            piece = color[0]
        elif piece_type == 'c':
            if color == 'red' and self.red_cats == 0:
                return False
            if color == 'blue' and self.blue_cats == 0:
                return False
            piece = color[0].upper()
        else:
            return False
        row, col = int(row), int(col)
        if not self.is_empty(row, col):
            return False

        self.grid[row][col] = piece
        if piece_type == 'k':
            if color == 'red':
                self.red_kittens -= 1
            else:
                self.blue_kittens -= 1
        else:
            if color == 'red':
                self.red_cats -= 1
            else:
                self.blue_cats -= 1

        self.push_adjacent(row, col)

        if self.has_three_in_row(color, 'cat') or self.has_eight_on_board(color, 'cat'):
            return True

        if piece_type == 'k':
            self.handle_kitten_upgrades(color)

        self.player_turn = 'blue' if color == 'red' else 'red'
        return False

    def handle_kitten_upgrades(self, color):
        three_kitten_rows = self.get_three_kitten_rows(color)
        if three_kitten_rows:
            if len(three_kitten_rows) == 1:
                self.upgrade_three_kittens(color, three_kitten_rows[0])
            else:
                self.ask_for_kitten_upgrade(color, three_kitten_rows)
        elif self.has_eight_on_board(color):
            self.upgrade_single_kitten(color)

    def get_three_kitten_rows(self, color):
        kitten = color[0]
        three_kitten_rows = []
        for row in range(6):
            for col in range(4):
                if all(self.grid[row][col+i] == kitten for i in range(3)):
                    three_kitten_rows.append((row, col))
        for col in range(6):
            for row in range(4):
                if all(self.grid[row+i][col] == kitten for i in range(3)):
                    three_kitten_rows.append((row, col))
        for row in range(4):
            for col in range(4):
                if all(self.grid[row+i][col+i] == kitten for i in range(3)):
                    three_kitten_rows.append((row, col))
        for row in range(4):
            for col in range(2, 6):
                if all(self.grid[row+i][col-i] == kitten for i in range(3)):
                    three_kitten_rows.append((row, col))
        return three_kitten_rows

    def ask_for_kitten_upgrade(self, color, three_kitten_rows):
        print(f"{color.capitalize()} player, you have multiple groups of three kittens in a row.")
        for i, (row, col) in enumerate(three_kitten_rows, start=1):
            print(f"Option {i}: Row {row}, Column {col}")
        choice = int(input("Enter the option number to upgrade to cats: "))
        row, col = three_kitten_rows[choice - 1]
        self.upgrade_three_kittens(color, (row, col))

    def upgrade_three_kittens(self, color, row_col):
        row, col = row_col
        kitten = color[0]
        cat = color[0].upper()
        for i in range(3):
            self.grid[row][col+i] = cat
        if color == 'red':
            self.red_kittens -= 3
            self.red_cats += 3
        else:
            self.blue_kittens -= 3
            self.blue_cats += 3

    def upgrade_single_kitten(self, color):
        kitten = color[0]
        cat = color[0].upper()
        for row in range(6):
            for col in range(6):
                if self.grid[row][col] == kitten:
                    self.grid[row][col] = cat
                    if color == 'red':
                        self.red_kittens -= 1
                        self.red_cats += 1
                    else:
                        self.blue_kittens -= 1
                        self.blue_cats += 1
                    return
                    
    def has_eight_on_board(self, color):
        count = sum(1 for row in self.grid for piece in row if piece and piece.lower() == color[0])
        return count == 8  

    def print_board(self):
        for row in self.grid:
            print(' '.join(piece or '.' for piece in row))
        print(f"Red kittens: {self.red_kittens}, Red cats: {self.red_cats}")
        print(f"Blue kittens: {self.blue_kittens}, Blue cats: {self.blue_cats}")
        print(f"Current player's turn: {self.player_turn}")

def play_game():
    board = Board()
    while True:
        board.print_board()
        color = board.player_turn
        move = input(f"{color.capitalize()} player, enter move (k/c row col): ")
        if board.place_piece(move):
            board.print_board()
            print(f"{color.capitalize()} player wins!")
            break

if __name__ == "__main__":
    play_game()