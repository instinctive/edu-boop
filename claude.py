import copy

class Board:
    def __init__(self):
        self.grid = [[None for _ in range(6)] for _ in range(6)]
        self.supplies = {'red': 8, 'blue': 8}
        self.player_turn = 'red'

    def is_empty(self, row, col):
        return self.grid[row][col] is None

    def place_piece(self, row, col, color):
        if not self.is_empty(row, col):
            return False

        self.grid[row][col] = f"{color}_kitten"
        self.supplies[color] -= 1
        self.push_adjacent(row, col)

        if self.has_three_in_row(color, 'cat'):
            return True

        if self.supplies[color] == 0:
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
        while self.is_on_board(new_row, new_col) and self.is_empty(new_row, new_col):
            new_row, new_col = new_row + dr, new_col + dc
        if self.is_on_board(new_row, new_col):
            self.grid[new_row][new_col] = self.grid[row][col]
            self.grid[row][col] = None
        else:
            color, piece = self.grid[row][col].split('_')
            self.grid[row][col] = None
            self.supplies[color] += 1

    def is_on_board(self, row, col):
        return 0 <= row < 6 and 0 <= col < 6

    def has_three_in_row(self, color, piece_type):
        for row in range(6):
            for col in range(4):
                if all(self.grid[row][col+i] == f"{color}_{piece_type}" for i in range(3)):
                    return True
        for col in range(6):
            for row in range(4):
                if all(self.grid[row+i][col] == f"{color}_{piece_type}" for i in range(3)):
                    return True
        for row in range(4):
            for col in range(4):
                if all(self.grid[row+i][col+i] == f"{color}_{piece_type}" for i in range(3)):
                    return True
        for row in range(4):
            for col in range(2, 6):
                if all(self.grid[row+i][col-i] == f"{color}_{piece_type}" for i in range(3)):
                    return True
        return False

    def has_eight_on_board(self, color):
        count = sum(1 for row in self.grid for piece in row if piece and piece.startswith(color))
        return count == 8

    def upgrade_three_kittens(self, color):
        for row in range(6):
            for col in range(4):
                if all(self.grid[row][col+i] == f"{color}_kitten" for i in range(3)):
                    for i in range(3):
                        self.grid[row][col+i] = f"{color}_cat"
                    self.supplies[color] += 3
                    return
        for col in range(6):
            for row in range(4):
                if all(self.grid[row+i][col] == f"{color}_kitten" for i in range(3)):
                    for i in range(3):
                        self.grid[row+i][col] = f"{color}_cat"
                    self.supplies[color] += 3
                    return
        for row in range(4):
            for col in range(4):
                if all(self.grid[row+i][col+i] == f"{color}_kitten" for i in range(3)):
                    for i in range(3):
                        self.grid[row+i][col+i] = f"{color}_cat"
                    self.supplies[color] += 3
                    return
        for row in range(4):
            for col in range(2, 6):
                if all(self.grid[row+i][col-i] == f"{color}_kitten" for i in range(3)):
                    for i in range(3):
                        self.grid[row+i][col-i] = f"{color}_cat"
                    self.supplies[color] += 3
                    return

    def upgrade_single_kitten(self, color):
        if self.supplies[color] > 0:
            for row in range(6):
                for col in range(6):
                    if self.grid[row][col] == f"{color}_kitten":
                        self.grid[row][col] = f"{color}_cat"
                        self.supplies[color] -= 1
                        return

    def print_board(self):
        for row in self.grid:
            print(' '.join(piece or '.' for piece in row))
        print(f"Red supply: {self.supplies['red']}, Blue supply: {self.supplies['blue']}")
        print(f"Current player's turn: {self.player_turn}")

def play_game():
    board = Board()
    while True:
        board.print_board()
        color = board.player_turn
        row = int(input(f"{color.capitalize()} player, enter row: "))
        col = int(input(f"{color.capitalize()} player, enter col: "))
        if board.place_piece(row, col, color):
            board.print_board()
            print(f"{color.capitalize()} player wins!")
            break

if __name__ == "__main__":
    play_game()