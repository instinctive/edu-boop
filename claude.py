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

        if self.has_three_in_row(color, 'cat'):
            return True

        if piece_type == 'k':
            if (color == 'red' and self.red_kittens == 0) or (color == 'blue' and self.blue_kittens == 0):
                if self.has_eight_on_board(color):
                    return True
                self.upgrade_three_kittens(color)
                self.upgrade_single_kitten(color)

        self.player_turn = 'blue' if color == 'red' else 'red'
        return False

    def push_adjacent(self, row, col):
        for dr in [-1, 0, 1]:
            for dc in [-1, 0, 1]:
                if dr == dc == 0:
                    continue
                new_row, new_col = row + dr, col + dc
                if self.is_on_board(new_row, new_col) and self.grid[new_row][new_col] is not None:
                    self.push_piece(new_row, new_col, dr, dc)

    def push_piece(self, row, col, dr, dc):
        new_row, new_col = row + dr, col + dc
        if self.is_on_board(new_row, new_col) and self.is_empty(new_row, new_col):
            self.grid[new_row][new_col] = self.grid[row][col]
            self.grid[row][col] = None
        else:
            piece = self.grid[row][col]
            self.grid[row][col] = None
            if piece == 'r':
                self.red_kittens += 1
            elif piece == 'R':
                self.red_cats += 1
            elif piece == 'b':
                self.blue_kittens += 1
            else:
                self.blue_cats += 1

    def is_on_board(self, row, col):
        return 0 <= row < 6 and 0 <= col < 6

    def has_three_in_row(self, color, piece_type):
        piece = color[0].upper() if piece_type == 'cat' else color[0]
        for row in range(6):
            for col in range(4):
                if all(self.grid[row][col+i] == piece for i in range(3)):
                    return True
        for col in range(6):
            for row in range(4):
                if all(self.grid[row+i][col] == piece for i in range(3)):
                    return True
        for row in range(4):
            for col in range(4):
                if all(self.grid[row+i][col+i] == piece for i in range(3)):
                    return True
        for row in range(4):
            for col in range(2, 6):
                if all(self.grid[row+i][col-i] == piece for i in range(3)):
                    return True
        return False

    def has_eight_on_board(self, color):
        count = sum(1 for row in self.grid for piece in row if piece and piece.lower() == color[0])
        return count == 8

    def upgrade_three_kittens(self, color):
        kitten = color[0]
        cat = color[0].upper()
        for row in range(6):
            for col in range(4):
                if all(self.grid[row][col+i] == kitten for i in range(3)):
                    for i in range(3):
                        self.grid[row][col+i] = cat
                    if color == 'red':
                        self.red_kittens -= 3
                        self.red_cats += 3
                    else:
                        self.blue_kittens -= 3
                        self.blue_cats += 3
                    return
        for col in range(6):
            for row in range(4):
                if all(self.grid[row+i][col] == kitten for i in range(3)):
                    for i in range(3):
                        self.grid[row+i][col] = cat
                    if color == 'red':
                        self.red_kittens -= 3
                        self.red_cats += 3
                    else:
                        self.blue_kittens -= 3
                        self.blue_cats += 3
                    return
        for row in range(4):
            for col in range(4):
                if all(self.grid[row+i][col+i] == kitten for i in range(3)):
                    for i in range(3):
                        self.grid[row+i][col+i] = cat
                    if color == 'red':
                        self.red_kittens -= 3
                        self.red_cats += 3
                    else:
                        self.blue_kittens -= 3
                        self.blue_cats += 3
                    return
        for row in range(4):
            for col in range(2, 6):
                if all(self.grid[row+i][col-i] == kitten for i in range(3)):
                    for i in range(3):
                        self.grid[row+i][col-i] = cat
                    if color == 'red':
                        self.red_kittens -= 3
                        self.red_cats += 3
                    else:
                        self.blue_kittens -= 3
                        self.blue_cats += 3
                    return

    def upgrade_single_kitten(self, color):
        kitten = color[0]
        cat = color[0].upper()
        if color == 'red' and self.red_cats > 0:
            for row in range(6):
                for col in range(6):
                    if self.grid[row][col] == kitten:
                        self.grid[row][col] = cat
                        self.red_kittens -= 1
                        self.red_cats -= 1
                        return
        elif color == 'blue' and self.blue_cats > 0:
            for row in range(6):
                for col in range(6):
                    if self.grid[row][col] == kitten:
                        self.grid[row][col] = cat
                        self.blue_kittens -= 1
                        self.blue_cats -= 1
                        return

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