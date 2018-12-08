from raspilights import *

RAINBOW = [RED, ORANGE, YELLOW, GREEN, BLUE, PURPLE]

def bubble_sort():
    grid = [random.choice(RAINBOW) for _ in all_pixels()]

    for passnum in range(len(grid)-1, 0, -1):
        for i in range(passnum):
            if compare(grid[i], grid[i+1]):
                grid[i], grid[i + 1] = grid[i + 1], grid[i]
        if passnum % 2 == 0:
            for i, color in enumerate(grid):
                set_pixel(i, color)
            show(0.1)

def compare(a, b):
    return RAINBOW.index(a) < RAINBOW.index(b)

if __name__ == '__main__':
    while True:
        bubble_sort()
