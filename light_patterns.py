from raspilights import *

def param_rainbow_cycle(t):
    c = all_colors()
    for _ in range(t):
        c = rotate(c)
    for i in all_pixels():
        set_pixel(i, c[i])

def rainbow_cycle():
    c = all_colors()
    for _ in all_colors():
        for i in all_pixels():
            set_pixel(i, c[i])
        c = rotate(c)
        show(0.05)

def param_rainbow_colors(t):
    c = all_colors()
    for _ in range(t):
        c = rotate(c)
    for i in all_pixels():
        set_pixel(i, c[0])

def param_brightness_decrease(t):
    step = 5
    set_all_pixels(WHITE)
    for _ in range(len(all_colors()) // step):
        for i in all_pixels():
            r, g, b = get_color(i)
            r = max(0, r - step)
            g = max(0, g - step)
            b = max(0, b - step)
            pixels.set_pixel(i, color(r, g, b))

def rainbow_colors():
    for color in all_colors():
        set_all_pixels(color)
        show(0.05)

def brightness_decrease(step=5):
    set_all_pixels(WHITE)
    for _ in range(len(all_colors()) // step):
        for i in all_pixels():
            r, g, b = get_color(i)
            r = max(0, r - step)
            g = max(0, g - step)
            b = max(0, b - step)
            pixels.set_pixel(i, color(r, g, b))
        show(0.01)

def brightness_increase(step=5):
    set_all_pixels(BLACK)
    for _ in range(len(all_colors()) // step):
        for i in all_pixels():
            r, g, b = get_color(i)
            r = max(0, r + step)
            g = max(0, g + step)
            b = max(0, b + step)
            pixels.set_pixel(i, color(r, g, b))
        show(0.01)

def movie_thing():
    for t in range(100):
        clear()
        for i in all_pixels():
            if i % 3 == t % 3:
                set_pixel(i, WHITE)
        show(0.05)

def candy_cane(background=WHITE, foreground=RED):
    for t in range(60):
        set_all_pixels(foreground)
        for i in all_pixels():
            if i % 7 == t % 7 or \
               i % 3 == t % 3 or \
               i % 4 == t % 4 or \
               i % 5 == t % 5:
                set_pixel(i, background)
        show(0.05)

def appear_from_back(color=RED):
    pos = 0
    for i in all_pixels():
        for j in reversed(all_pixels()):
            clear()
            for k in range(i):
                set_pixel(k, color)
            set_pixel(j, color)
            show(0.001)
            if j <= i:
                break

def alternating(c1=color(255,0,0), c2=color(0,255,0), chunkSize=5, times=20):
    for _ in range(times):
        for i in all_pixels():
            if i % (chunkSize * 2) < chunkSize:
                set_pixel(i, c1)
            else:
                set_pixel(i, c2)
        show(0.2)
        c1, c2 = c2, c1

def red_green_alternating():
    alternating(c1=RED, c2=GREEN, chunkSize=5, times=20)

def default_colors():
    # GRAY can also be spelled GREY
    # PINK can also be spelled MAGENTA
    for color in [BLACK, RED, GREEN, BLUE, YELLOW, CYAN, PINK, WHITE, GRAY, PURPLE,
            DARK_RED, DARK_GREEN, DARK_BLUE, DARK_YELLOW, DARK_CYAN,
            LIGHT_RED, LIGHT_GREEN, LIGHT_BLUE, LIGHT_YELLOW, LIGHT_PINK, LIGHT_CYAN,
            ORANGE, SALMON, AQUAMARINE, LIME, AUBERGINE, CERULEAN]:
        set_all_pixels(color)
        show(0.25)

if __name__ == '__main__':
    set_mode('software')

    procedures = [brightnessDecrease, rainbowCycle]

    proc = random.choice(procedures)
    for t in range(100):
        proc(t)
        show(0.05)

    for t in range(100):
        param_rainbow_colors(t)
        show(0.05)

    for t in range(100):
        param_brightness_decrease(t)
        show(0.05)

    red_green_alternating()
    default_colors()
    candy_cane(background=WHITE, foreground=RED)
    brightness_increase()
    brightness_decrease()
    rainbow_cycle()
    movie_thing()
    alternating(c1=random_color(),
              c2=random_color(),
              chunkSize=random.randint(2,20))
    rainbow_colors()
    appear_from_back()

    print('done!')
