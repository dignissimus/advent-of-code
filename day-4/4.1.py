BOARDS = 100

class Board:
    def __init__(self):
        self.unmarked = set()
        self.win = False

        self.rc = [0]*5
        self.cc = [0]*5
        self.locations = {}

    def score(self, number):
        return sum(self.unmarked) * number

def main():
    numbers = map(int, input().split(","))
    boards = []
    
    for i in range(BOARDS):
        input()
        board = Board()
        for j in range(5):
            row = list(map(int, input().strip().split()))
            board.unmarked |= set(row)
            for k, n in enumerate(row):
                board.locations[n] = (j, k)
        boards.append(board)

    for number in numbers:
        for board in boards:
            if not board.win and number in board.unmarked:
                board.unmarked -= {number}
                j, k = board.locations[number]
                board.rc[j] += 1
                board.cc[k] += 1

                if 5 in [board.rc[j], board.cc[k]]:
                    board.win = True
                    print(board.score(number))           
                    exit()
    
if __name__ == "__main__":
    main()
