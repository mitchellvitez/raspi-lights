from raspilights import *
import random

def splash():
    set_all_pixels(BLACK)

    i = random.randint(0, PIXEL_COUNT - 1)
    for t in range(100):
        set_all_pixels(BLACK)
        brightness = 255 - 10 * t
        if brightness <= 0:
            break

        if i - t >= 0:
            set_pixel(i - t, (0, 0, brightness))

        if i + t < PIXEL_COUNT:
            set_pixel(i + t, (0, 0, brightness))

        show(0.1)

if __name__ == '__main__':
    while True:
        splash()
