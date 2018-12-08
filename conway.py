from raspilights import *

from copy import copy
import random

def conway():
    grid = [random.choice([WHITE, BLACK]) for _ in all_pixels()]
    for _ in range(100):

        # show grid on pixel array
        for i, color in enumerate(grid):
            set_pixel(i, color)
        show(0.1)

        # need to copy so we're not overwriting cells before we count their neighbors
        new_grid = copy(grid)

        for i, p in enumerate(grid):
            # get four closest neighbors in one dimension
            neighbors = [ grid[(i-2) % PIXEL_COUNT]
                        , grid[(i-1) % PIXEL_COUNT]
                        , grid[(i+1) % PIXEL_COUNT]
                        , grid[(i+2) % PIXEL_COUNT]
                        ]

            # number of neighbors with light lit
            live_neighbors = sum([1 for n in neighbors if n == WHITE])

            # conway's game of life rules
            if p == BLACK:
                if live_neighbors == 2 or live_neighbors == 3:
                    new_grid[i] = WHITE
            else:
                if live_neighbors != 2 and live_neighbors != 4:
                    new_grid[i] = BLACK

        grid = copy(new_grid)

if __name__ == '__main__':
    while True:
        conway()
